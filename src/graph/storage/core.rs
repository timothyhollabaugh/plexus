use geometry::Geometry;
use graph::mesh::{Edge, Face, Vertex};
use graph::storage::convert::{AsStorage, AsStorageMut};
use graph::storage::{Storage, Topological};

pub trait Bind<T, M>
where
    T: Topological,
    M: AsStorage<T>,
{
    type Output;

    fn bind(self, source: M) -> Self::Output;
}

/// Abstract and ephemeral core mesh storage.
pub struct Core<V = (), E = (), F = ()>(pub(in graph) V, pub(in graph) E, pub(in graph) F);

impl Core {
    pub fn empty() -> Self {
        Core((), (), ())
    }
}

impl<V, E, F> Core<V, E, F> {
    pub(in graph) fn as_storage<T>(&self) -> &Storage<T>
    where
        Self: AsStorage<T>,
        T: Topological,
    {
        AsStorage::<T>::as_storage(self)
    }

    pub(in graph) fn as_storage_mut<T>(&mut self) -> &mut Storage<T>
    where
        Self: AsStorageMut<T>,
        T: Topological,
    {
        AsStorageMut::<T>::as_storage_mut(self)
    }
}

impl<V, E, F, G> AsStorage<Vertex<G>> for Core<V, E, F>
where
    V: AsStorage<Vertex<G>>,
    G: Geometry,
{
    fn as_storage(&self) -> &Storage<Vertex<G>> {
        self.0.as_storage()
    }
}

impl<V, E, F, G> AsStorage<Edge<G>> for Core<V, E, F>
where
    E: AsStorage<Edge<G>>,
    G: Geometry,
{
    fn as_storage(&self) -> &Storage<Edge<G>> {
        self.1.as_storage()
    }
}

impl<V, E, F, G> AsStorage<Face<G>> for Core<V, E, F>
where
    F: AsStorage<Face<G>>,
    G: Geometry,
{
    fn as_storage(&self) -> &Storage<Face<G>> {
        self.2.as_storage()
    }
}

impl<V, E, F, G> AsStorageMut<Vertex<G>> for Core<V, E, F>
where
    V: AsStorageMut<Vertex<G>>,
    G: Geometry,
{
    fn as_storage_mut(&mut self) -> &mut Storage<Vertex<G>> {
        self.0.as_storage_mut()
    }
}

impl<V, E, F, G> AsStorageMut<Edge<G>> for Core<V, E, F>
where
    E: AsStorageMut<Edge<G>>,
    G: Geometry,
{
    fn as_storage_mut(&mut self) -> &mut Storage<Edge<G>> {
        self.1.as_storage_mut()
    }
}

impl<V, E, F, G> AsStorageMut<Face<G>> for Core<V, E, F>
where
    F: AsStorageMut<Face<G>>,
    G: Geometry,
{
    fn as_storage_mut(&mut self) -> &mut Storage<Face<G>> {
        self.2.as_storage_mut()
    }
}

impl<V, E, F, G> Bind<Vertex<G>, V> for Core<(), E, F>
where
    V: AsStorage<Vertex<G>>,
    G: Geometry,
{
    type Output = Core<V, E, F>;

    fn bind(self, vertices: V) -> Self::Output {
        let Core(_, edges, faces) = self;
        Core(vertices, edges, faces)
    }
}

impl<V, E, F, G> Bind<Edge<G>, E> for Core<V, (), F>
where
    E: AsStorage<Edge<G>>,
    G: Geometry,
{
    type Output = Core<V, E, F>;

    fn bind(self, edges: E) -> Self::Output {
        let Core(vertices, _, faces) = self;
        Core(vertices, edges, faces)
    }
}

impl<V, E, F, G> Bind<Face<G>, F> for Core<V, E, ()>
where
    F: AsStorage<Face<G>>,
    G: Geometry,
{
    type Output = Core<V, E, F>;

    fn bind(self, faces: F) -> Self::Output {
        let Core(vertices, edges, _) = self;
        Core(vertices, edges, faces)
    }
}
