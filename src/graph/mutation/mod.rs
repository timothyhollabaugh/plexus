pub mod edge;
pub mod face;
pub mod vertex;

use failure::Error;
use std::fmt::Debug;
use std::mem;
use std::ops::{Deref, DerefMut};

use geometry::Geometry;
use graph::mesh::{Consistent, Inconsistent, Mesh};
use graph::mutation::face::FaceMutation;
use graph::storage::{EdgeKey, FaceKey, VertexKey};

pub use self::face::{FaceInsertCache, FaceRemoveCache};

// TODO: It may be a good idea to raise `GraphError::TopologyNotFound` errors as
//       soon as possible. Presence could be checked immediately, allowing for
//       these errors to be recoverable (instead of, for example, raising such
//       an error well after mutations have been performed).
// TODO: Consider moving mutation like `extrude_with_cache` out of the
//       `topology` module and into the `mutation` module.

pub trait Mode<G>
where
    G: Geometry,
{
    //type Mutant: AsRef<Mesh<G, Consistent>> + AsMut<Mesh<G, Consistent>>;
    type Mutant;
}

pub trait Mutate<G>: Commit<G> + Mode<G>
where
    G: Geometry,
{
    fn mutate(mutant: Self::Mutant) -> Self;
}

pub trait Commit<G>: Mode<G> + Sized
where
    G: Geometry,
{
    type Error: Debug;

    fn commit(self) -> Result<Self::Mutant, Self::Error>;

    fn commit_with<F, T, E>(mut self, f: F) -> Result<(Self::Mutant, T), Self::Error>
    where
        F: FnOnce(&mut Self) -> Result<T, E>,
        E: Into<Self::Error>,
    {
        let output = f(&mut self);
        match output {
            Ok(value) => self.commit().map(|mutant| (mutant, value)),
            Err(error) => {
                self.abort();
                Err(error.into())
            }
        }
    }

    fn abort(self) {}
}

pub struct Replace<'a, M, G>
where
    M: Commit<G> + Mode<G, Mutant = Mesh<G, Consistent>> + Mutate<G>,
    G: 'a + Geometry,
{
    mutation: Option<(&'a mut Mesh<G, Consistent>, M)>,
}

impl<'a, M, G> Replace<'a, M, G>
where
    M: Commit<G> + Mode<G, Mutant = Mesh<G, Consistent>> + Mutate<G>,
    G: 'a + Geometry,
{
    pub fn replace(mesh: <Self as Mode<G>>::Mutant, replacement: Mesh<G, Consistent>) -> Self {
        let mutant = mem::replace(mesh, replacement);
        Replace {
            mutation: Some((mesh, M::mutate(mutant))),
        }
    }

    fn drain(&mut self) -> (&'a mut Mesh<G, Consistent>, M) {
        self.mutation.take().unwrap()
    }

    fn drain_and_commit(&mut self) -> Result<<Self as Mode<G>>::Mutant, <Self as Commit<G>>::Error> {
        let (mesh, mutation) = self.drain();
        let mutant = mutation.commit()?;
        mem::replace(mesh, mutant);
        Ok(mesh)
    }

    fn drain_and_abort(&mut self) {
        let (_, mutation) = self.drain();
        mutation.abort();
    }
}

impl<'a, M, G> Commit<G> for Replace<'a, M, G>
where
    M: Commit<G> + Mode<G, Mutant = Mesh<G, Consistent>> + Mutate<G>,
    G: 'a + Geometry,
{
    type Error = <M as Commit<G>>::Error;

    fn commit(mut self) -> Result<<Self as Mode<G>>::Mutant, Self::Error> {
        let mutant = self.drain_and_commit();
        mem::forget(self);
        mutant
    }

    fn abort(mut self) {
        self.drain_and_abort();
        mem::forget(self);
    }
}

impl<'a, M, G> Deref for Replace<'a, M, G>
where
    M: Commit<G> + Mode<G, Mutant = Mesh<G, Consistent>> + Mutate<G>,
    G: 'a + Geometry,
{
    type Target = M;

    fn deref(&self) -> &Self::Target {
        &self.mutation.as_ref().unwrap().1
    }
}

impl<'a, M, G> DerefMut for Replace<'a, M, G>
where
    M: Commit<G> + Mode<G, Mutant = Mesh<G, Consistent>> + Mutate<G>,
    G: 'a + Geometry,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.mutation.as_mut().unwrap().1
    }
}

impl<'a, M, G> Drop for Replace<'a, M, G>
where
    M: Commit<G> + Mode<G, Mutant = Mesh<G, Consistent>> + Mutate<G>,
    G: 'a + Geometry,
{
    fn drop(&mut self) {
        self.drain_and_abort();
    }
}

impl<'a, M, G> Mode<G> for Replace<'a, M, G>
where
    M: Commit<G> + Mode<G, Mutant = Mesh<G, Consistent>> + Mutate<G>,
    G: 'a + Geometry,
{
    type Mutant = &'a mut Mesh<G, Consistent>;
}

impl<'a, M, G> Mutate<G> for Replace<'a, M, G>
where
    M: Commit<G> + Mode<G, Mutant = Mesh<G, Consistent>> + Mutate<G>,
    G: 'a + Geometry,
{
    fn mutate(mutant: Self::Mutant) -> Self {
        Self::replace(mutant, Mesh::empty())
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum Key {
    Vertex(VertexKey),
    Edge(EdgeKey),
    Face(FaceKey),
}

impl From<VertexKey> for Key {
    fn from(key: VertexKey) -> Self {
        Key::Vertex(key)
    }
}

impl From<EdgeKey> for Key {
    fn from(key: EdgeKey) -> Self {
        Key::Edge(key)
    }
}

impl From<FaceKey> for Key {
    fn from(key: FaceKey) -> Self {
        Key::Face(key)
    }
}

/// Mesh mutation.
///
/// Mutates a `Mesh`.
pub struct Mutation<G>
where
    G: Geometry,
{
    mutation: FaceMutation<G>,
    touched: HashSet<Key>,
}

impl<G> Mutation<G>
where
    G: Geometry,
{
    pub fn replace(
        mesh: &mut Mesh<G, Consistent>,
        replacement: Mesh<G, Consistent>,
    ) -> Replace<Self, G> {
        Replace::replace(mesh, replacement)
    }

    pub fn mutant(&self) -> &Mesh<G, Inconsistent> {
        self.mutation.mutant()
    }
}

impl<G> Commit<G> for Mutation<G>
where
    G: Geometry,
{
    type Error = Error;

    fn commit(self) -> Result<Self::Mutant, Self::Error> {
        self.mutation
            .commit()
            .map(|mutant| mutant.into_consistency())
    }
}

impl<G> Deref for Mutation<G>
where
    G: Geometry,
{
    type Target = FaceMutation<G>;

    fn deref(&self) -> &Self::Target {
        &self.mutation
    }
}

impl<G> DerefMut for Mutation<G>
where
    G: Geometry,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.mutation
    }
}

impl<G> Mode<G> for Mutation<G>
where
    G: Geometry,
{
    type Mutant = Mesh<G, Consistent>;
}

impl<G> Mutate<G> for Mutation<G>
where
    G: Geometry,
{
    fn mutate(mutant: Self::Mutant) -> Self {
        Mutation {
            mutation: FaceMutation::mutate(mutant.into_consistency()),
        }
    }
}
