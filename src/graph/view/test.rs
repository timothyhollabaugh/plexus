use std::marker::PhantomData;
use std::mem;
use std::ops::{Deref, DerefMut};

use geometry::Geometry;
use graph::mesh::{Edge, Face, Mesh, Vertex};
use graph::storage::{
    AsStorage, AsStorageMut, Bind, EdgeKey, FaceKey, Storage, Topological, VertexKey,
};
use graph::view::{IteratorExt, OrphanEdgeView, OrphanFaceView, OrphanView, View};

pub struct EdgeView<M, G>
where
    M: AsStorage<Edge<G>>,
    G: Geometry,
{
    key: EdgeKey,
    storage: M,
    phantom: PhantomData<G>,
}

impl<M, G> EdgeView<M, G>
where
    M: AsStorage<Edge<G>>,
    G: Geometry,
{
    fn new(key: EdgeKey, storage: M) -> Self {
        EdgeView {
            key,
            storage,
            phantom: PhantomData,
        }
    }
}

impl<M, G> Deref for EdgeView<M, G>
where
    M: AsStorage<Edge<G>>,
    G: Geometry,
{
    type Target = Edge<G>;

    fn deref(&self) -> &Self::Target {
        panic!()
    }
}

pub struct FaceView<M, G>
where
    M: AsStorage<Face<G>>,
    G: Geometry,
{
    key: FaceKey,
    storage: M,
    phantom: PhantomData<G>,
}

impl<M, G> FaceView<M, G>
where
    M: AsStorage<Face<G>>,
    G: Geometry,
{
    fn new(key: FaceKey, storage: M) -> Self {
        FaceView {
            key,
            storage,
            phantom: PhantomData,
        }
    }
}

impl<M, G> Deref for FaceView<M, G>
where
    M: AsStorage<Face<G>>,
    G: Geometry,
{
    type Target = Face<G>;

    fn deref(&self) -> &Self::Target {
        panic!()
    }
}

/// Do **not** use this type directly. Use `VertexRef` and `VertexMut` instead.
///
/// This type is only re-exported so that its members are shown in
/// documentation. See this issue:
/// <https://github.com/rust-lang/rust/issues/39437>
pub struct VertexView<M, G>
where
    M: AsStorage<Vertex<G>>,
    G: Geometry,
{
    key: VertexKey,
    storage: M,
    phantom: PhantomData<G>,
}

/// Storage.
impl<M, G> VertexView<M, G>
where
    M: AsStorage<Vertex<G>>,
    G: Geometry,
{
    pub(in graph) fn bind<T, N>(self, storage: N) -> VertexView<<M as Bind<T, N>>::Output, G>
    where
        T: Topological,
        M: Bind<T, N>,
        M::Output: AsStorage<Vertex<G>>,
        N: AsStorage<T>,
    {
        let VertexView {
            key,
            storage: origin,
            ..
        } = self;
        VertexView {
            key,
            storage: origin.bind(storage),
            phantom: PhantomData,
        }
    }

    pub(in graph) fn as_storage<T>(&self) -> &Storage<T>
    where
        T: Topological,
        M: AsStorage<T>,
    {
        AsStorage::<T>::as_storage(&self.storage)
    }

    pub(in graph) fn as_storage_mut<T>(&mut self) -> &mut Storage<T>
    where
        T: Topological,
        M: AsStorageMut<T>,
    {
        AsStorageMut::<T>::as_storage_mut(&mut self.storage)
    }
}

impl<M, G> VertexView<M, G>
where
    M: AsStorage<Vertex<G>>,
    G: Geometry,
{
    pub(in graph) fn new(key: VertexKey, storage: M) -> Self {
        VertexView {
            key,
            storage,
            phantom: PhantomData,
        }
    }

    pub fn key(&self) -> VertexKey {
        self.key
    }
}

impl<M, G> VertexView<M, G>
where
    M: AsStorage<Edge<G>> + AsStorage<Vertex<G>>,
    G: Geometry,
{
    pub fn into_outgoing_edge(self) -> EdgeView<M, G> {
        let key = self.edge.unwrap();
        let VertexView { storage, .. } = self;
        EdgeView::new(key, storage)
    }

    pub fn outgoing_edge(&self) -> EdgeView<&M, G> {
        let key = self.edge.unwrap();
        let storage = &self.storage;
        EdgeView::new(key, storage)
    }

    pub fn incoming_edges(&self) -> impl Iterator<Item = EdgeView<&M, G>> {
        let key = self.edge;
        let storage = &self.storage;
        EdgeCirculator::new(key, storage)
            .map_with_ref(|circulator, key| EdgeView::new(key, circulator.storage))
    }
}

impl<M, G> VertexView<M, G>
where
    M: AsStorage<Edge<G>> + AsStorage<Face<G>> + AsStorage<Vertex<G>>,
    G: Geometry,
{
    pub fn neighboring_faces(&self) -> impl Iterator<Item = FaceView<&M, G>> {
        let key = self.edge;
        let storage = &self.storage;
        FaceCirculator::from(EdgeCirculator::new(key, storage))
            .map_with_ref(|circulator, key| FaceView::new(key, circulator.input.storage))
    }
}

impl<M, G> VertexView<M, G>
where
    M: AsStorage<Edge<G>> + AsStorageMut<Edge<G>> + AsStorage<Vertex<G>>,
    G: Geometry,
{
    pub fn outgoing_edge_mut(&mut self) -> EdgeView<&mut M, G> {
        let key = self.edge.unwrap();
        let storage = &mut self.storage;
        EdgeView::new(key, storage)
    }

    pub fn outgoing_orphan_edge(&mut self) -> OrphanEdgeView<G> {
        let key = self.edge.unwrap();
        OrphanEdgeView::new(self.storage.as_storage_mut().get_mut(&key).unwrap(), key)
    }

    pub fn incoming_orphan_edges<'a>(&'a mut self) -> impl Iterator<Item = OrphanEdgeView<'a, G>> {
        let key = self.edge;
        EdgeCirculator::new(key, &mut self.storage).map_with_mut(|circulator, key| {
            OrphanEdgeView::new(
                unsafe {
                    // Apply `'a` to the autoref from `as_storage_mut` and
                    // `get_mut`.
                    mem::transmute::<&'_ mut Edge<G>, &'a mut Edge<G>>(
                        circulator.storage.as_storage_mut().get_mut(&key).unwrap(),
                    )
                },
                key,
            )
        })
    }
}

impl<M, G> VertexView<M, G>
where
    M: AsStorage<Edge<G>> + AsStorage<Face<G>> + AsStorageMut<Face<G>> + AsStorage<Vertex<G>>,
    G: Geometry,
{
    pub fn neighboring_orphan_faces<'a>(
        &'a mut self,
    ) -> impl Iterator<Item = OrphanFaceView<'a, G>> {
        let key = self.edge;
        FaceCirculator::from(EdgeCirculator::new(key, &mut self.storage)).map_with_mut(
            |circulator, key| {
                OrphanFaceView::new(
                    unsafe {
                        // Apply `'a` to the autoref from `as_storage_mut` and
                        // `get_mut`.
                        mem::transmute::<&'_ mut Face<G>, &'a mut Face<G>>(
                            circulator
                                .input
                                .storage
                                .as_storage_mut()
                                .get_mut(&key)
                                .unwrap(),
                        )
                    },
                    key,
                )
            },
        )
    }
}

impl<M, G> Deref for VertexView<M, G>
where
    M: AsStorage<Vertex<G>>,
    G: Geometry,
{
    type Target = Vertex<G>;

    fn deref(&self) -> &Self::Target {
        self.storage.as_storage().get(&self.key).unwrap()
    }
}

impl<M, G> DerefMut for VertexView<M, G>
where
    M: AsStorage<Vertex<G>> + AsStorageMut<Vertex<G>>,
    G: Geometry,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.storage.as_storage_mut().get_mut(&self.key).unwrap()
    }
}

impl<M, G> Clone for VertexView<M, G>
where
    M: AsStorage<Vertex<G>> + Clone,
    G: Geometry,
{
    fn clone(&self) -> Self {
        VertexView {
            key: self.key,
            storage: self.storage.clone(),
            phantom: PhantomData,
        }
    }
}

impl<M, G> Copy for VertexView<M, G>
where
    M: AsStorage<Vertex<G>> + Copy,
    G: Geometry,
{
}

impl<M, G> View<M, G> for VertexView<M, G>
where
    M: AsRef<Mesh<G>> + AsStorage<Vertex<G>>,
    G: Geometry,
{
    type Topology = Vertex<G>;

    fn from_mesh(key: <Self::Topology as Topological>::Key, mesh: M) -> Self {
        VertexView::new(key, mesh)
    }
}

/// Do **not** use this type directly. Use `OrphanVertexMut` instead.
///
/// This type is only re-exported so that its members are shown in
/// documentation. See this issue:
/// <https://github.com/rust-lang/rust/issues/39437>
pub struct OrphanVertexView<'a, G>
where
    G: 'a + Geometry,
{
    key: VertexKey,
    vertex: &'a mut Vertex<G>,
}

impl<'a, G> OrphanVertexView<'a, G>
where
    G: 'a + Geometry,
{
    pub(in graph) fn new(key: VertexKey, vertex: &'a mut Vertex<G>) -> Self {
        OrphanVertexView {
            key: key,
            vertex: vertex,
        }
    }

    pub fn key(&self) -> VertexKey {
        self.key
    }
}

impl<'a, G> Deref for OrphanVertexView<'a, G>
where
    G: 'a + Geometry,
{
    type Target = <Self as OrphanView<'a, G>>::Topology;

    fn deref(&self) -> &Self::Target {
        &*self.vertex
    }
}

impl<'a, G> DerefMut for OrphanVertexView<'a, G>
where
    G: 'a + Geometry,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.vertex
    }
}

impl<'a, G> OrphanView<'a, G> for OrphanVertexView<'a, G>
where
    G: 'a + Geometry,
{
    type Topology = Vertex<G>;

    fn from_topology(
        key: <Self::Topology as Topological>::Key,
        topology: &'a mut Self::Topology,
    ) -> Self {
        OrphanVertexView::new(key, topology)
    }
}

pub struct EdgeCirculator<'a, M, G>
where
    M: 'a + AsStorage<Edge<G>>,
    G: 'a + Geometry,
{
    storage: M,
    edge: Option<EdgeKey>,
    breadcrumb: Option<EdgeKey>,
    phantom: PhantomData<&'a G>,
}

impl<'a, M, G> EdgeCirculator<'a, M, G>
where
    M: 'a + AsStorage<Edge<G>>,
    G: 'a + Geometry,
{
    fn new(ab: Option<EdgeKey>, storage: M) -> Self {
        EdgeCirculator {
            storage,
            edge: ab,
            breadcrumb: ab,
            phantom: PhantomData,
        }
    }
}

impl<'a, M, G> Iterator for EdgeCirculator<'a, M, G>
where
    M: 'a + AsStorage<Edge<G>>,
    G: 'a + Geometry,
{
    type Item = EdgeKey;

    fn next(&mut self) -> Option<Self::Item> {
        self.edge
            .map(|outgoing| self.storage.as_storage().get(&outgoing).unwrap())
            .and_then(|outgoing| outgoing.opposite)
            .and_then(|incoming| {
                let outgoing = self.storage.as_storage().get(&incoming).unwrap().next;
                self.breadcrumb.map(|_| {
                    if self.breadcrumb == outgoing {
                        self.breadcrumb = None;
                    }
                    else {
                        self.edge = outgoing;
                    }
                    incoming
                })
            })
    }
}

pub struct FaceCirculator<'a, M, G>
where
    M: 'a + AsStorage<Edge<G>> + AsStorage<Face<G>>,
    G: 'a + Geometry,
{
    input: EdgeCirculator<'a, M, G>,
}

impl<'a, M, G> From<EdgeCirculator<'a, M, G>> for FaceCirculator<'a, M, G>
where
    M: 'a + AsStorage<Edge<G>> + AsStorage<Face<G>>,
    G: 'a + Geometry,
{
    fn from(input: EdgeCirculator<'a, M, G>) -> Self {
        FaceCirculator { input }
    }
}

impl<'a, M, G> Iterator for FaceCirculator<'a, M, G>
where
    M: 'a + AsStorage<Edge<G>> + AsStorage<Face<G>>,
    G: 'a + Geometry,
{
    type Item = FaceKey;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(edge) = self.input.next() {
            if let Some(face) = AsStorage::<Edge<G>>::as_storage(&self.input.storage)
                .get(&edge)
                .and_then(|edge| edge.face)
            {
                return Some(face);
            }
            else {
                // Skip edges with no face. This can occur within non-enclosed
                // meshes.
                continue;
            }
        }
        None
    }
}
