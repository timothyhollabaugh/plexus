use arrayvec::ArrayVec;
use failure::{Error, Fail};
use itertools::Itertools;
use num::{Integer, NumCast, Unsigned};
use std::collections::HashMap;
use std::hash::Hash;
use std::iter::FromIterator;
use std::marker::PhantomData;

use buffer::MeshBuffer;
use generate::{
    self, Arity, FromIndexer, HashIndexer, IndexVertices, Indexer, IntoVertices, MapVerticesInto,
    Quad,
};
use geometry::convert::{FromGeometry, FromInteriorGeometry, IntoGeometry, IntoInteriorGeometry};
use geometry::Geometry;
use graph::geometry::FaceCentroid;
use graph::mutation::{ModalMutation, Mutation};
use graph::storage::alias::InnerKey;
use graph::storage::{
    AsStorage, AsStorageMut, Bind, EdgeKey, FaceKey, Storage, Topological, VertexKey,
};
use graph::view::{
    EdgeMut, EdgeRef, FaceMut, FaceRef, OrphanEdgeMut, OrphanFaceMut, OrphanVertexMut, OrphanView,
    VertexMut, VertexRef, View,
};
use graph::{GraphError, Perimeter};
use BoolExt;

// TODO: derivative panics on `pub(in graph)`, so this type uses `pub(super)`.
#[derivative(Debug, Hash)]
#[derive(Clone, Derivative)]
pub struct Vertex<G>
where
    G: Geometry,
{
    #[derivative(Debug = "ignore", Hash = "ignore")]
    pub geometry: G::Vertex,
    pub(super) edge: Option<EdgeKey>,
}

impl<G> Vertex<G>
where
    G: Geometry,
{
    pub(in graph) fn new(geometry: G::Vertex) -> Self {
        Vertex {
            geometry: geometry,
            edge: None,
        }
    }
}

impl<G, H> FromInteriorGeometry<Vertex<H>> for Vertex<G>
where
    G: Geometry,
    G::Vertex: FromGeometry<H::Vertex>,
    H: Geometry,
{
    fn from_interior_geometry(vertex: Vertex<H>) -> Self {
        Vertex {
            geometry: vertex.geometry.into_geometry(),
            edge: vertex.edge,
        }
    }
}

impl<G> Topological for Vertex<G>
where
    G: Geometry,
{
    type Key = VertexKey;
    type Attribute = G::Vertex;
}

// TODO: derivative panics on `pub(in graph)`, so this type uses `pub(super)`.
#[derivative(Debug, Hash)]
#[derive(Clone, Derivative)]
pub struct Edge<G>
where
    G: Geometry,
{
    #[derivative(Debug = "ignore", Hash = "ignore")]
    pub geometry: G::Edge,
    pub(super) vertex: VertexKey,
    pub(super) opposite: Option<EdgeKey>,
    pub(super) next: Option<EdgeKey>,
    pub(super) previous: Option<EdgeKey>,
    pub(super) face: Option<FaceKey>,
}

impl<G> Edge<G>
where
    G: Geometry,
{
    pub(in graph) fn new(vertex: VertexKey, geometry: G::Edge) -> Self {
        Edge {
            geometry: geometry,
            vertex: vertex,
            opposite: None,
            next: None,
            previous: None,
            face: None,
        }
    }
}

impl<G, H> FromInteriorGeometry<Edge<H>> for Edge<G>
where
    G: Geometry,
    G::Edge: FromGeometry<H::Edge>,
    H: Geometry,
{
    fn from_interior_geometry(edge: Edge<H>) -> Self {
        Edge {
            geometry: edge.geometry.into_geometry(),
            vertex: edge.vertex,
            opposite: edge.opposite,
            next: edge.next,
            previous: edge.previous,
            face: edge.face,
        }
    }
}

impl<G> Topological for Edge<G>
where
    G: Geometry,
{
    type Key = EdgeKey;
    type Attribute = G::Edge;
}

// TODO: derivative panics on `pub(in graph)`, so this type uses `pub(super)`.
#[derivative(Debug, Hash)]
#[derive(Clone, Derivative)]
pub struct Face<G>
where
    G: Geometry,
{
    #[derivative(Debug = "ignore", Hash = "ignore")]
    pub geometry: G::Face,
    pub(super) edge: EdgeKey,
}

impl<G> Face<G>
where
    G: Geometry,
{
    pub(in graph) fn new(edge: EdgeKey, geometry: G::Face) -> Self {
        Face {
            geometry: geometry,
            edge: edge,
        }
    }
}

impl<G, H> FromInteriorGeometry<Face<H>> for Face<G>
where
    G: Geometry,
    G::Face: FromGeometry<H::Face>,
    H: Geometry,
{
    fn from_interior_geometry(face: Face<H>) -> Self {
        Face {
            geometry: face.geometry.into_geometry(),
            edge: face.edge,
        }
    }
}

impl<G> Topological for Face<G>
where
    G: Geometry,
{
    type Key = FaceKey;
    type Attribute = G::Face;
}

/// Vertex-bounded region.
///
/// A unique set of vertices that forms a bounded region that can represent a
/// face in a mesh.
///
/// See `Mesh::region`.
#[derive(Clone, Copy, Debug)]
pub struct Region<'a>(&'a [VertexKey]);

impl<'a> Region<'a> {
    pub fn as_vertices(&self) -> &[VertexKey] {
        self.0
    }
}

/// Vertex-bounded region connectivity.
///
/// Describes the per-vertex edge connectivity of a region bounded by a set of
/// vertices. This is primarily used to connect exterior edges when a face is
/// inserted into a mesh.
pub type Connectivity = HashMap<VertexKey, Vec<EdgeKey>>;

pub type Singularity = (VertexKey, Vec<FaceKey>);

/// Abstract and ephemeral core mesh storage.
pub struct Core<V = (), E = (), F = ()>(V, E, F);

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

/// Half-edge graph representation of a mesh.
///
/// Provides topological data in the form of vertices, half-edges, and faces. A
/// half-edge is directed from one vertex to another, with an opposing
/// half-edge joining the vertices in the other direction.
///
/// `Mesh`es expose topological views, which can be used to traverse and
/// manipulate topology and geometry.
///
/// See the module documentation for more details.
pub type Mesh<G = ()> = Core<Storage<Vertex<G>>, Storage<Edge<G>>, Storage<Face<G>>>;

/// Storage.
impl<G> Mesh<G>
where
    G: Geometry,
{
    pub(in graph) fn as_disjoint_storage_mut(
        &mut self,
    ) -> (
        &mut Storage<Vertex<G>>,
        &mut Storage<Edge<G>>,
        &mut Storage<Face<G>>,
    ) {
        let Core(ref mut vertices, ref mut edges, ref mut faces) = self;
        (vertices, edges, faces)
    }
}

impl<G> Mesh<G>
where
    G: Geometry,
{
    /// Creates an empty `Mesh`.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use plexus::graph::Mesh;
    ///
    /// let mut mesh = Mesh::<()>::new();
    /// ```
    pub fn new() -> Self {
        Core(Storage::new(), Storage::new(), Storage::new())
    }

    /// Creates an empty `Mesh`.
    ///
    /// Underlying storage has zero capacity and does not allocate until the
    /// first insertion.
    pub(in graph) fn empty() -> Self {
        Core(Storage::empty(), Storage::empty(), Storage::empty())
    }

    /// Creates a `Mesh` from raw index and vertex buffers. The arity of the
    /// polygons in the index buffer must be known and constant.
    ///
    /// # Errors
    ///
    /// Returns an error if the arity of the index buffer is not constant, any
    /// index is out of bounds, or there is an error inserting topology into
    /// the mesh.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # extern crate nalgebra;
    /// # extern crate plexus;
    /// use nalgebra::Point3;
    /// use plexus::generate::LruIndexer;
    /// use plexus::generate::sphere::UvSphere;
    /// use plexus::graph::Mesh;
    /// use plexus::prelude::*;
    ///
    /// # fn main() {
    /// let (indeces, positions) = UvSphere::new(16, 16)
    ///     .polygons_with_position()
    ///     .triangulate()
    ///     .flat_index_vertices(LruIndexer::with_capacity(256));
    /// let mut mesh = Mesh::<Point3<f64>>::from_raw_buffers(indeces, positions, 3);
    /// # }
    /// ```
    pub fn from_raw_buffers<I, J>(indeces: I, vertices: J, arity: usize) -> Result<Self, Error>
    where
        I: IntoIterator<Item = usize>,
        J: IntoIterator,
        J::Item: IntoGeometry<G::Vertex>,
    {
        let mut mutation = Mutation::batch(Mesh::new());
        let vertices = vertices
            .into_iter()
            .map(|vertex| mutation.insert_vertex(vertex.into_geometry()))
            .collect::<Vec<_>>();
        for face in &indeces.into_iter().chunks(arity) {
            let face = face.collect::<Vec<_>>();
            if face.len() != arity {
                return Err(GraphError::ArityConflict {
                    expected: arity,
                    actual: face.len(),
                }.context("index buffer lenght is not a multiple of arity")
                    .into());
            }
            let mut perimeter = Vec::with_capacity(arity);
            for index in face {
                perimeter.push(
                    *vertices
                        .get(index)
                        .ok_or_else(|| Error::from(GraphError::TopologyNotFound))?,
                );
            }
            mutation.insert_face(&perimeter, Default::default())?;
        }
        mutation.commit()
    }

    /// Gets the number of vertices in the mesh.
    pub fn vertex_count(&self) -> usize {
        self.as_storage::<Vertex<G>>().len()
    }

    /// Gets an immutable view of the vertex with the given key.
    pub fn vertex(&self, vertex: VertexKey) -> Option<VertexRef<G>> {
        self.as_storage::<Vertex<G>>()
            .get(&vertex)
            .map(|_| VertexRef::new(self, vertex))
    }

    /// Gets a mutable view of the vertex with the given key.
    pub fn vertex_mut(&mut self, vertex: VertexKey) -> Option<VertexMut<G>> {
        self.as_storage_mut::<Vertex<G>>()
            .contains_key(&vertex)
            .into_some(VertexMut::new(self, vertex))
    }

    pub(in graph) fn orphan_vertex_mut(&mut self, vertex: VertexKey) -> Option<OrphanVertexMut<G>> {
        self.as_storage_mut::<Vertex<G>>()
            .get_mut(&vertex)
            .map(|topology| OrphanVertexMut::new(topology, vertex))
    }

    /// Gets an iterator of immutable views over the vertices in the mesh.
    pub fn vertices(&self) -> impl Iterator<Item = VertexRef<G>> {
        Iter::new(self, self.as_storage::<Vertex<G>>().iter())
    }

    /// Gets an iterator of orphan views over the vertices in the mesh.
    ///
    /// Because this only yields orphan views, only geometry can be mutated.
    /// For topological mutations, collect the necessary keys and use
    /// `vertex_mut` instead.
    pub fn vertices_mut(&mut self) -> impl Iterator<Item = OrphanVertexMut<G>> {
        IterMut::new(self.as_storage_mut::<Vertex<G>>().iter_mut())
    }

    /// Gets the number of edges in the mesh.
    pub fn edge_count(&self) -> usize {
        self.as_storage::<Edge<G>>().len()
    }

    /// Gets an immutable view of the edge with the given key.
    pub fn edge(&self, edge: EdgeKey) -> Option<EdgeRef<G>> {
        self.as_storage::<Edge<G>>()
            .get(&edge)
            .map(|_| EdgeRef::new(self, edge))
    }

    /// Gets a mutable view of the edge with the given key.
    pub fn edge_mut(&mut self, edge: EdgeKey) -> Option<EdgeMut<G>> {
        self.as_storage_mut::<Edge<G>>()
            .contains_key(&edge)
            .into_some(EdgeMut::new(self, edge))
    }

    pub(in graph) fn orphan_edge_mut(&mut self, edge: EdgeKey) -> Option<OrphanEdgeMut<G>> {
        self.as_storage_mut::<Edge<G>>()
            .get_mut(&edge)
            .map(|topology| OrphanEdgeMut::new(topology, edge))
    }

    /// Gets an iterator of immutable views over the edges in the mesh.
    pub fn edges(&self) -> impl Iterator<Item = EdgeRef<G>> {
        Iter::new(self, self.as_storage::<Edge<G>>().iter())
    }

    /// Gets an iterator of orphan views over the edges in the mesh.
    ///
    /// Because this only yields orphan views, only geometry can be mutated.
    /// For topological mutations, collect the necessary keys and use
    /// `edge_mut` instead.
    pub fn edges_mut(&mut self) -> impl Iterator<Item = OrphanEdgeMut<G>> {
        IterMut::new(self.as_storage_mut::<Edge<G>>().iter_mut())
    }

    /// Gets the number of faces in the mesh.
    pub fn face_count(&self) -> usize {
        self.as_storage::<Face<G>>().len()
    }

    /// Gets an immutable view of the face with the given key.
    pub fn face(&self, face: FaceKey) -> Option<FaceRef<G>> {
        self.as_storage::<Face<G>>()
            .get(&face)
            .map(|_| FaceRef::new(self, face))
    }

    /// Gets a mutable view of the face with the given key.
    pub fn face_mut(&mut self, face: FaceKey) -> Option<FaceMut<G>> {
        self.as_storage_mut::<Face<G>>()
            .contains_key(&face)
            .into_some(FaceMut::new(self, face))
    }

    pub(in graph) fn orphan_face_mut(&mut self, face: FaceKey) -> Option<OrphanFaceMut<G>> {
        self.as_storage_mut::<Face<G>>()
            .get_mut(&face)
            .map(|topology| OrphanFaceMut::new(topology, face))
    }

    /// Gets an iterator of immutable views over the faces in the mesh.
    pub fn faces(&self) -> impl Iterator<Item = FaceRef<G>> {
        Iter::new(self, self.as_storage::<Face<G>>().iter())
    }

    /// Gets an iterator of orphan views over the faces in the mesh.
    ///
    /// Because this only yields orphan views, only geometry can be mutated.
    /// For topological mutations, collect the necessary keys and use
    /// `face_mut` instead.
    pub fn faces_mut(&mut self) -> impl Iterator<Item = OrphanFaceMut<G>> {
        IterMut::new(self.as_storage_mut::<Face<G>>().iter_mut())
    }

    /// Triangulates the mesh, tesselating all faces into triangles.
    pub fn triangulate(&mut self) -> Result<(), Error>
    where
        G: FaceCentroid<Centroid = <G as Geometry>::Vertex> + Geometry,
    {
        let faces = self
            .as_storage::<Face<G>>()
            .keys()
            .map(|key| FaceKey::from(*key))
            .collect::<Vec<_>>();
        for face in faces {
            let face = FaceMut::new(self, face);
            face.triangulate()?;
        }
        Ok(())
    }

    /// Creates a mesh buffer from the mesh.
    ///
    /// The buffer is created using the vertex geometry of each unique vertex.
    ///
    /// # Errors
    ///
    /// Returns an error if the mesh does not have constant arity. Typically, a
    /// mesh is triangulated before being converted to a mesh buffer.
    pub fn to_mesh_buffer_by_vertex<N, V>(&self) -> Result<MeshBuffer<N, V>, Error>
    where
        G::Vertex: IntoGeometry<V>,
        N: Copy + Integer + NumCast + Unsigned,
    {
        self.to_mesh_buffer_by_vertex_with(|vertex| vertex.geometry.clone().into_geometry())
    }

    /// Creates a mesh buffer from the mesh.
    ///
    /// The buffer is created using each unique vertex, which is converted into
    /// the buffer geometry by the given function.
    ///
    /// # Errors
    ///
    /// Returns an error if the mesh does not have constant arity. Typically, a
    /// mesh is triangulated before being converted to a mesh buffer.
    pub fn to_mesh_buffer_by_vertex_with<N, V, F>(
        &self,
        mut f: F,
    ) -> Result<MeshBuffer<N, V>, Error>
    where
        N: Copy + Integer + NumCast + Unsigned,
        F: FnMut(VertexRef<G>) -> V,
    {
        let (keys, vertices) = {
            let mut keys = HashMap::with_capacity(self.vertex_count());
            let mut vertices = Vec::with_capacity(self.vertex_count());
            for (n, vertex) in self.vertices().enumerate() {
                keys.insert(vertex.key(), n);
                vertices.push(f(vertex));
            }
            (keys, vertices)
        };
        let indeces = {
            let arity = match self.faces().nth(0) {
                Some(face) => face.arity(),
                _ => 0,
            };
            let mut indeces = Vec::with_capacity(arity * self.face_count());
            for face in self.faces() {
                if face.arity() != arity {
                    return Err(GraphError::ArityNonConstant.into());
                }
                for vertex in face.vertices() {
                    indeces.push(N::from(keys[&vertex.key()]).unwrap());
                }
            }
            indeces
        };
        MeshBuffer::from_raw_buffers(indeces, vertices)
    }

    /// Creates a mesh buffer from the mesh.
    ///
    /// The buffer is created using the vertex geometry of each face. Shared
    /// vertices are included for each face to which they belong.
    ///
    /// # Errors
    ///
    /// Returns an error if the mesh does not have constant arity. Typically, a
    /// mesh is triangulated before being converted to a mesh buffer.
    pub fn to_mesh_buffer_by_face<N, V>(&self) -> Result<MeshBuffer<N, V>, Error>
    where
        G::Vertex: IntoGeometry<V>,
        N: Copy + Integer + NumCast + Unsigned,
    {
        self.to_mesh_buffer_by_face_with(|_, vertex| vertex.geometry.clone().into_geometry())
    }

    /// Creates a mesh buffer from the mesh.
    ///
    /// The buffer is created from each face, which is converted into the
    /// buffer geometry by the given function.
    ///
    /// # Errors
    ///
    /// Returns an error if the mesh does not have constant arity. Typically, a
    /// mesh is triangulated before being converted to a mesh buffer.
    pub fn to_mesh_buffer_by_face_with<N, V, F>(&self, mut f: F) -> Result<MeshBuffer<N, V>, Error>
    where
        N: Copy + Integer + NumCast + Unsigned,
        F: FnMut(FaceRef<G>, VertexRef<G>) -> V,
    {
        let vertices = {
            let arity = match self.faces().nth(0) {
                Some(face) => face.arity(),
                _ => 0,
            };
            let mut vertices = Vec::with_capacity(arity * self.face_count());
            for face in self.faces() {
                if face.arity() != arity {
                    return Err(GraphError::ArityNonConstant.into());
                }
                for vertex in face.vertices() {
                    vertices.push(f(face, vertex));
                }
            }
            vertices
        };
        MeshBuffer::from_raw_buffers(
            // TODO: Cannot use the bound `N: Step`, which is unstable.
            (0..vertices.len()).map(|index| N::from(index).unwrap()),
            vertices,
        )
    }

    pub(in graph) fn region<'a>(&self, vertices: &'a [VertexKey]) -> Result<Region<'a>, Error> {
        // A face requires at least three vertices (edges). This invariant
        // should be maintained by any code that is able to mutate the mesh,
        // such that code manipulating faces (via `FaceView`) may assume this
        // is true. Panics resulting from faces with fewer than three vertices
        // are bugs.
        if vertices.len() < 3 {
            return Err(GraphError::TopologyMalformed
                .context("non-polygonal arity")
                .into());
        }
        if vertices.len() != vertices.iter().unique().count() {
            return Err(GraphError::TopologyMalformed
                .context("non-manifold bounds")
                .into());
        }
        // Fail if any vertex is not present.
        if vertices.iter().any(|vertex| self.vertex(*vertex).is_none()) {
            return Err(GraphError::TopologyNotFound.into());
        }
        // Fail if the interior is already occupied by a face.
        if vertices
            .perimeter()
            .flat_map(|ab| self.edge(ab.into()))
            .any(|edge| edge.face().is_some())
        {
            return Err(GraphError::TopologyConflict
                .context("interior edge has face")
                .into());
        }
        Ok(Region(vertices))
    }

    pub(in graph) fn region_connectivity(
        &self,
        region: Region,
    ) -> ((Connectivity, Connectivity), Option<Singularity>) {
        // Get the outgoing and incoming edges of the vertices forming the
        // perimeter.
        let outgoing = region
            .as_vertices()
            .iter()
            .map(|vertex| {
                (
                    *vertex,
                    self.vertex(*vertex)
                        .unwrap()
                        .incoming_edges()
                        .map(|edge| edge.opposite_edge().key())
                        .collect::<Vec<_>>(),
                )
            })
            .collect::<HashMap<_, _>>();
        let incoming = region
            .as_vertices()
            .iter()
            .map(|vertex| {
                (
                    *vertex,
                    self.vertex(*vertex)
                        .unwrap()
                        .incoming_edges()
                        .map(|edge| edge.key())
                        .collect::<Vec<_>>(),
                )
            })
            .collect::<HashMap<_, _>>();
        // If only one vertex has any outgoing edges, then this face shares
        // exactly one vertex with other faces and is therefore non-manifold.
        //
        // This kind of non-manifold is not supported, but sometimes occurs
        // during a batch mutation. Details of the singularity vertex are
        // emitted and handled by calling code, either raising an error or
        // waiting to validate after a batch mutation is complete.
        let singularity = {
            let mut outgoing = outgoing.iter().filter(|&(_, edges)| !edges.is_empty());
            if let Some((vertex, _)) = outgoing.next() {
                outgoing.next().map_or_else(
                    || {
                        let faces = self
                            .vertex(*vertex)
                            .unwrap()
                            .faces()
                            .map(|face| face.key())
                            .collect::<Vec<_>>();
                        Some((*vertex, faces))
                    },
                    |_| None,
                )
            }
            else {
                None
            }
        };
        ((incoming, outgoing), singularity)
    }
}

impl<G> AsRef<Mesh<G>> for Mesh<G>
where
    G: Geometry,
{
    fn as_ref(&self) -> &Self {
        self
    }
}

impl<G> AsMut<Mesh<G>> for Mesh<G>
where
    G: Geometry,
{
    fn as_mut(&mut self) -> &mut Self {
        self
    }
}

impl<G> Default for Mesh<G>
where
    G: Geometry,
{
    fn default() -> Self {
        Mesh::new()
    }
}

impl<G, H> FromInteriorGeometry<Mesh<H>> for Mesh<G>
where
    G: Geometry,
    G::Vertex: FromGeometry<H::Vertex>,
    G::Edge: FromGeometry<H::Edge>,
    G::Face: FromGeometry<H::Face>,
    H: Geometry,
{
    fn from_interior_geometry(mesh: Mesh<H>) -> Self {
        let Core(vertices, edges, faces) = mesh;
        Core(
            vertices.map_values_into(|vertex| vertex.into_interior_geometry()),
            edges.map_values_into(|edge| edge.into_interior_geometry()),
            faces.map_values_into(|face| face.into_interior_geometry()),
        )
    }
}

impl<G, P> FromIndexer<P, P> for Mesh<G>
where
    G: Geometry,
    P: MapVerticesInto<usize> + generate::Topological,
    P::Output: IntoVertices,
    <P::Output as IntoVertices>::Output: AsRef<[usize]>,
    P::Vertex: IntoGeometry<G::Vertex>,
{
    fn from_indexer<I, N>(input: I, indexer: N) -> Self
    where
        I: IntoIterator<Item = P>,
        N: Indexer<P, P::Vertex>,
    {
        let mut mutation = Mutation::batch(Mesh::new());
        let (indeces, vertices) = input.into_iter().index_vertices(indexer);
        let vertices = vertices
            .into_iter()
            .map(|vertex| mutation.insert_vertex(vertex.into_geometry()))
            .collect::<Vec<_>>();
        for face in indeces {
            let face = face.into_vertices();
            // The topology with the greatest arity emitted by indexing is a
            // quad. Avoid allocations by using an `ArrayVec`.
            let mut perimeter = ArrayVec::<[_; Quad::<usize>::ARITY]>::new();
            for index in face {
                perimeter.push(vertices[index]);
            }
            mutation
                .insert_face(&perimeter, Default::default())
                .unwrap();
        }
        mutation.commit().unwrap()
    }
}

impl<G, P> FromIterator<P> for Mesh<G>
where
    G: Geometry,
    P: MapVerticesInto<usize> + generate::Topological,
    P::Output: IntoVertices,
    <P::Output as IntoVertices>::Output: AsRef<[usize]>,
    P::Vertex: Eq + Hash + IntoGeometry<G::Vertex>,
{
    fn from_iter<I>(input: I) -> Self
    where
        I: IntoIterator<Item = P>,
    {
        Self::from_indexer(input, HashIndexer::default())
    }
}

pub struct Iter<'a, I, T, G>
where
    I: 'a + Iterator<Item = (&'a InnerKey<T::Topology>, &'a T::Topology)>,
    T: 'a + View<&'a Mesh<G>, G>,
    T::Topology: 'a,
    G: 'a + Geometry,
{
    mesh: &'a Mesh<G>,
    input: I,
    phantom: PhantomData<T>,
}

impl<'a, I, T, G> Iter<'a, I, T, G>
where
    I: 'a + Iterator<Item = (&'a InnerKey<T::Topology>, &'a T::Topology)>,
    T: 'a + View<&'a Mesh<G>, G>,
    G: 'a + Geometry,
{
    fn new(mesh: &'a Mesh<G>, input: I) -> Self {
        Iter {
            mesh: mesh,
            input: input,
            phantom: PhantomData,
        }
    }
}

impl<'a, I, T, G> Iterator for Iter<'a, I, T, G>
where
    I: 'a + Iterator<Item = (&'a InnerKey<T::Topology>, &'a T::Topology)>,
    T: 'a + View<&'a Mesh<G>, G>,
    G: 'a + Geometry,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.input
            .next()
            .map(|entry| T::from_mesh(self.mesh, (*entry.0).into()))
    }
}

pub struct IterMut<'a, I, T, G>
where
    I: 'a + Iterator<Item = (&'a InnerKey<T::Topology>, &'a mut T::Topology)>,
    T: 'a + OrphanView<'a, G>,
    G: 'a + Geometry,
{
    input: I,
    phantom: PhantomData<(T, G)>,
}

impl<'a, I, T, G> IterMut<'a, I, T, G>
where
    I: 'a + Iterator<Item = (&'a InnerKey<T::Topology>, &'a mut T::Topology)>,
    T: 'a + OrphanView<'a, G>,
    G: 'a + Geometry,
{
    fn new(input: I) -> Self {
        IterMut {
            input: input,
            phantom: PhantomData,
        }
    }
}

impl<'a, I, T, G> Iterator for IterMut<'a, I, T, G>
where
    I: 'a + Iterator<Item = (&'a InnerKey<T::Topology>, &'a mut T::Topology)>,
    T: 'a + OrphanView<'a, G>,
    G: 'a + Geometry,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.input.next().map(|entry| {
            T::from_topology(
                unsafe {
                    use std::mem;

                    // This should be safe, because the use of this iterator
                    // requires a mutable borrow of the source mesh with
                    // lifetime `'a`. Therefore, the (disjoint) geometry data
                    // within the mesh should also be valid over the lifetime
                    // '`a'.
                    mem::transmute::<_, &'a mut T::Topology>(entry.1)
                },
                (*entry.0).into(),
            )
        })
    }
}

#[cfg(test)]
mod tests {
    use nalgebra::{Point2, Point3, Vector3};
    use num::Zero;
    use std::collections::HashSet;

    use generate::*;
    use geometry::*;
    use graph::mutation::{ModalMutation, Mutation};
    use graph::*;

    #[test]
    fn collect_topology_into_mesh() {
        let mesh = sphere::UvSphere::new(3, 2)
            .polygons_with_position() // 6 triangles, 18 vertices.
            .collect::<Mesh<Point3<f32>>>();

        assert_eq!(5, mesh.vertex_count());
        assert_eq!(18, mesh.edge_count());
        assert_eq!(6, mesh.face_count());
    }

    #[test]
    fn iterate_mesh_topology() {
        let mut mesh = sphere::UvSphere::new(4, 2)
            .polygons_with_position() // 8 triangles, 24 vertices.
            .collect::<Mesh<Point3<f32>>>();

        assert_eq!(6, mesh.vertices().count());
        assert_eq!(24, mesh.edges().count());
        assert_eq!(8, mesh.faces().count());
        for vertex in mesh.vertices() {
            // Every vertex is connected to 4 triangles with 4 (incoming)
            // edges. Traversal of topology should be possible.
            assert_eq!(4, vertex.incoming_edges().count());
        }
        for mut vertex in mesh.vertices_mut() {
            // Geometry should be mutable.
            vertex.geometry += Vector3::zero();
        }
    }

    #[test]
    fn non_manifold_error_deferred() {
        let mesh = sphere::UvSphere::new(32, 32)
            .polygons_with_position()
            .triangulate()
            .collect::<Mesh<Point3<f32>>>();
        // This conversion will join faces by a single vertex, but ultimately
        // creates a manifold.
        mesh.to_mesh_buffer_by_face_with::<usize, Point3<f32>, _>(|_, vertex| vertex.geometry)
            .unwrap();
    }

    #[test]
    fn error_on_non_manifold_mesh() {
        // Construct a mesh with a "fan" of three triangles sharing the same
        // edge along the Z-axis. The edge would have three associated faces,
        // which should not be possible.
        let mesh = Mesh::<Point3<i32>>::from_raw_buffers(
            vec![0, 1, 2, 0, 1, 3, 0, 1, 4],
            vec![(0, 0, 1), (0, 0, -1), (1, 0, 0), (0, 1, 0), (1, 1, 0)],
            3,
        );

        assert!(match *mesh
            .err()
            .unwrap()
            .root_cause()
            .downcast_ref::<GraphError>()
            .unwrap()
        {
            GraphError::TopologyConflict => true,
            _ => false,
        });
    }

    #[test]
    fn error_on_singularity_mesh() {
        // Construct a mesh with three non-neighboring triangles sharing a
        // single vertex.
        let mesh = Mesh::<Point3<i32>>::from_raw_buffers(
            vec![0, 1, 2, 0, 3, 4, 0, 5, 6],
            vec![
                (0, 0, 0),
                (1, -1, 0),
                (-1, -1, 0),
                (-3, 1, 0),
                (-2, 1, 0),
                (2, 1, 0),
                (3, 1, 0),
            ],
            3,
        );

        assert!(match *mesh
            .err()
            .unwrap()
            .root_cause()
            .downcast_ref::<GraphError>()
            .unwrap()
        {
            GraphError::TopologyMalformed => true,
            _ => false,
        });

        // Construct a mesh with three triangles forming a rectangle, where one
        // vertex (at the origin) is shared by all three triangles.
        let mut mesh = Mesh::<Point2<i32>>::from_raw_buffers(
            vec![0, 1, 3, 1, 4, 3, 1, 2, 4],
            vec![(-1, 0), (0, 0), (1, 0), (-1, 1), (1, 1)],
            3,
        ).unwrap();
        // TODO: Create a shared testing geometry that allows topology to be
        //       marked and more easily located. Finding very specific geometry
        //       like this is cumbersome.
        // Find the "center" triangle and use an immediate mutation to remove
        // it. This creates a singularity, with the two remaining triangles
        // sharing no edges but having a single common vertex.
        let geometry = &[(0, 0), (1, 1), (-1, 1)]
            .iter()
            .cloned()
            .collect::<HashSet<_>>();
        let key = mesh
            .faces()
            .find(|face| {
                face.vertices()
                    .map(|vertex| vertex.geometry.clone())
                    .map(|position| (position.x, position.y))
                    .collect::<HashSet<_>>() == *geometry
            })
            .unwrap()
            .key();
        let mut mutation = Mutation::immediate(&mut mesh);
        assert!(match *mutation
            .remove_face(key)
            .err()
            .unwrap()
            .root_cause()
            .downcast_ref::<GraphError>()
            .unwrap()
        {
            GraphError::TopologyConflict => true,
            _ => false,
        });
    }

    // This test is a sanity check for mesh iterators, topological views, and
    // the unsafe transmutations used to coerce lifetimes.
    #[test]
    fn read_write_geometry_ref() {
        impl Attribute for f32 {}

        struct ValueGeometry;

        impl Geometry for ValueGeometry {
            type Vertex = Point3<f32>;
            type Edge = ();
            type Face = f32;
        }

        // Create a mesh with a floating point value associated with each face.
        // Use a mutable iterator to write to the geometry of each face.
        let mut mesh = sphere::UvSphere::new(4, 4)
            .polygons_with_position()
            .collect::<Mesh<ValueGeometry>>();
        let value = 3.14;
        for mut face in mesh.faces_mut() {
            face.geometry = value;
        }

        // Read the geometry of each face using an immutable iterator to ensure
        // it is what we expect.
        for face in mesh.faces() {
            assert_eq!(value, face.geometry);
        }
    }
}
