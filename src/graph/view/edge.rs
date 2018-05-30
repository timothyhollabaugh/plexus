use arrayvec::ArrayVec;
use failure::Error;
use std::marker::PhantomData;
use std::ops::{Add, Deref, DerefMut, Mul};

use geometry::convert::AsPosition;
use geometry::Geometry;
use graph::geometry::alias::{ScaledEdgeLateral, VertexPosition};
use graph::geometry::{EdgeLateral, EdgeMidpoint};
use graph::mesh::{Edge, Face, Mesh, Vertex};
use graph::mutation::{ModalMutation, Mutation};
use graph::storage::{EdgeKey, FaceKey, Topological, VertexKey};
use graph::view::{FaceView, OrphanFaceView, OrphanVertexView, OrphanView, VertexView, View};
use graph::{GraphError, Perimeter};

/// Do **not** use this type directly. Use `EdgeRef` and `EdgeMut` instead.
///
/// This type is only re-exported so that its members are shown in
/// documentation. See this issue:
/// <https://github.com/rust-lang/rust/issues/39437>
pub struct EdgeView<M, G>
where
    M: AsRef<Mesh<G>>,
    G: Geometry,
{
    mesh: M,
    key: EdgeKey,
    phantom: PhantomData<G>,
}

impl<M, G> EdgeView<M, G>
where
    M: AsRef<Mesh<G>>,
    G: Geometry,
{
    pub(in graph) fn new(mesh: M, edge: EdgeKey) -> Self {
        EdgeView {
            mesh: mesh,
            key: edge,
            phantom: PhantomData,
        }
    }

    pub fn key(&self) -> EdgeKey {
        self.key
    }

    pub fn to_key_topology(&self) -> EdgeKeyTopology {
        EdgeKeyTopology::new(self.key, self.key.to_vertex_keys())
    }

    pub fn source_vertex(&self) -> VertexView<&Mesh<G>, G> {
        let (vertex, _) = self.key.to_vertex_keys();
        VertexView::new(self.mesh.as_ref(), vertex)
    }

    pub fn into_source_vertex(self) -> VertexView<M, G> {
        let (vertex, _) = self.key.to_vertex_keys();
        let mesh = self.mesh;
        VertexView::new(mesh, vertex)
    }

    pub fn destination_vertex(&self) -> VertexView<&Mesh<G>, G> {
        VertexView::new(self.mesh.as_ref(), self.vertex)
    }

    pub fn into_destination_vertex(self) -> VertexView<M, G> {
        let vertex = self.vertex;
        let mesh = self.mesh;
        VertexView::new(mesh, vertex)
    }

    pub fn opposite_edge(&self) -> EdgeView<&Mesh<G>, G> {
        self.raw_opposite_edge().unwrap()
    }

    pub fn into_opposite_edge(self) -> Self {
        self.into_raw_opposite_edge().unwrap()
    }

    pub fn next_edge(&self) -> EdgeView<&Mesh<G>, G> {
        self.raw_next_edge().unwrap()
    }

    pub fn into_next_edge(self) -> Self {
        self.into_raw_next_edge().unwrap()
    }

    pub fn previous_edge(&self) -> EdgeView<&Mesh<G>, G> {
        self.raw_previous_edge().unwrap()
    }

    pub fn into_previous_edge(self) -> Self {
        self.into_raw_previous_edge().unwrap()
    }

    pub fn face(&self) -> Option<FaceView<&Mesh<G>, G>> {
        self.face
            .map(|face| FaceView::new(self.mesh.as_ref(), face))
    }

    pub fn into_face(self) -> Option<FaceView<M, G>> {
        let face = self.face;
        let mesh = self.mesh;
        face.map(|face| FaceView::new(mesh, face))
    }

    pub fn boundary_edge(&self) -> Option<EdgeView<&Mesh<G>, G>> {
        use BoolExt;

        if self.is_boundary_edge() {
            Some(self.with_mesh_ref())
        }
        else {
            let opposite = self.opposite_edge();
            opposite.is_boundary_edge().into_some(opposite)
        }
    }

    pub fn into_boundary_edge(self) -> Option<Self> {
        use BoolExt;

        if self.is_boundary_edge() {
            Some(self)
        }
        else {
            let opposite = self.into_opposite_edge();
            opposite.is_boundary_edge().into_some(opposite)
        }
    }

    pub fn is_boundary_edge(&self) -> bool {
        self.face().is_none()
    }

    pub fn vertices(&self) -> VertexCirculator<&Mesh<G>, G> {
        let (a, b) = self.key.to_vertex_keys();
        VertexCirculator::new(self.mesh.as_ref(), ArrayVec::from([b, a]))
    }

    pub fn faces(&self) -> FaceCirculator<&Mesh<G>, G> {
        FaceCirculator::new(
            self.mesh.as_ref(),
            self.face()
                .iter()
                .map(|face| face.key())
                .chain(self.opposite_edge().face().map(|face| face.key()))
                .collect(),
        )
    }

    // Resolve the `M` parameter to a concrete reference.
    #[allow(dead_code)]
    fn with_mesh_ref(&self) -> EdgeView<&Mesh<G>, G> {
        EdgeView::new(self.mesh.as_ref(), self.key)
    }
}

/// Raw API.
impl<M, G> EdgeView<M, G>
where
    M: AsRef<Mesh<G>>,
    G: Geometry,
{
    pub(in graph) fn raw_opposite_edge(&self) -> Option<EdgeView<&Mesh<G>, G>> {
        self.opposite
            .map(|opposite| EdgeView::new(self.mesh.as_ref(), opposite))
    }

    pub(in graph) fn into_raw_opposite_edge(self) -> Option<Self> {
        let opposite = self.opposite;
        let mesh = self.mesh;
        opposite.map(|opposite| EdgeView::new(mesh, opposite))
    }

    pub(in graph) fn raw_next_edge(&self) -> Option<EdgeView<&Mesh<G>, G>> {
        self.next
            .map(|next| EdgeView::new(self.mesh.as_ref(), next))
    }

    pub(in graph) fn into_raw_next_edge(self) -> Option<Self> {
        let next = self.next;
        let mesh = self.mesh;
        next.map(|next| EdgeView::new(mesh, next))
    }

    pub(in graph) fn raw_previous_edge(&self) -> Option<EdgeView<&Mesh<G>, G>> {
        self.previous
            .map(|previous| EdgeView::new(self.mesh.as_ref(), previous))
    }

    pub(in graph) fn into_raw_previous_edge(self) -> Option<Self> {
        let previous = self.previous;
        let mesh = self.mesh;
        previous.map(|previous| EdgeView::new(mesh, previous))
    }
}

impl<M, G> EdgeView<M, G>
where
    M: AsRef<Mesh<G>> + AsMut<Mesh<G>>,
    G: Geometry,
{
    pub fn opposite_edge_mut(&mut self) -> OrphanEdgeView<G> {
        self.raw_opposite_edge_mut().unwrap()
    }

    pub fn next_edge_mut(&mut self) -> OrphanEdgeView<G> {
        self.raw_next_edge_mut().unwrap()
    }

    pub fn previous_edge_mut(&mut self) -> OrphanEdgeView<G> {
        self.raw_previous_edge_mut().unwrap()
    }

    pub fn source_vertex_mut(&mut self) -> OrphanVertexView<G> {
        let (vertex, _) = self.key().to_vertex_keys();
        self.mesh.as_mut().orphan_vertex_mut(vertex).unwrap()
    }

    pub fn destination_vertex_mut(&mut self) -> OrphanVertexView<G> {
        let vertex = self.vertex;
        self.mesh.as_mut().orphan_vertex_mut(vertex).unwrap()
    }

    pub fn face_mut(&mut self) -> Option<OrphanFaceView<G>> {
        let face = self.face;
        face.map(move |face| self.mesh.as_mut().orphan_face_mut(face).unwrap())
    }

    pub fn boundary_edge_mut(&mut self) -> Option<OrphanEdgeView<G>> {
        use BoolExt;

        if self.is_boundary_edge() {
            Some(self.mesh.as_mut().orphan_edge_mut(self.key).unwrap())
        }
        else {
            self.opposite_edge()
                .is_boundary_edge()
                .into_some(self.opposite_edge_mut())
        }
    }

    // Resolve the `M` parameter to a concrete reference.
    #[allow(dead_code)]
    fn with_mesh_mut(&mut self) -> EdgeView<&mut Mesh<G>, G> {
        EdgeView::new(self.mesh.as_mut(), self.key)
    }
}

impl<'a, G> EdgeView<&'a mut Mesh<G>, G>
where
    G: Geometry,
{
    // TODO: Rename this to something like "extend". It is very similar to
    //       `extrude`. Terms like "join" or "merge" are better suited for
    //       directly joining two adjacent faces over a shared edge.
    pub fn join(self, destination: EdgeKey) -> Result<EdgeView<&'a mut Mesh<G>, G>, Error> {
        let EdgeView {
            mesh, key: source, ..
        } = self;
        let mut mutation = Mutation::immediate(mesh);
        let edge = join(&mut mutation, source, destination)?;
        Ok(EdgeView::new(mutation.commit(), edge))
    }
}

/// Raw API.
impl<M, G> EdgeView<M, G>
where
    M: AsRef<Mesh<G>> + AsMut<Mesh<G>>,
    G: Geometry,
{
    pub(in graph) fn raw_opposite_edge_mut(&mut self) -> Option<OrphanEdgeView<G>> {
        let opposite = self.opposite;
        opposite.map(move |opposite| self.mesh.as_mut().orphan_edge_mut(opposite).unwrap())
    }

    pub(in graph) fn raw_next_edge_mut(&mut self) -> Option<OrphanEdgeView<G>> {
        let next = self.next;
        next.map(move |next| self.mesh.as_mut().orphan_edge_mut(next).unwrap())
    }

    pub(in graph) fn raw_previous_edge_mut(&mut self) -> Option<OrphanEdgeView<G>> {
        let previous = self.previous;
        previous.map(move |previous| self.mesh.as_mut().orphan_edge_mut(previous).unwrap())
    }
}

impl<M, G> EdgeView<M, G>
where
    M: AsRef<Mesh<G>>,
    G: EdgeMidpoint + Geometry,
{
    pub fn midpoint(&self) -> Result<G::Midpoint, Error> {
        G::midpoint(self.with_mesh_ref())
    }
}

impl<'a, G> EdgeView<&'a mut Mesh<G>, G>
where
    G: EdgeMidpoint + Geometry,
    G::Vertex: AsPosition,
{
    pub fn split(self) -> Result<VertexView<&'a mut Mesh<G>, G>, Error>
    where
        G: EdgeMidpoint<Midpoint = VertexPosition<G>>,
    {
        let EdgeView { mesh, key: ab, .. } = self;
        let mut mutation = Mutation::immediate(mesh);
        let vertex = split(&mut mutation, ab)?;
        Ok(VertexView::new(mutation.commit(), vertex))
    }
}

impl<M, G> EdgeView<M, G>
where
    M: AsRef<Mesh<G>>,
    G: Geometry + EdgeLateral,
{
    pub fn lateral(&self) -> Result<G::Lateral, Error> {
        G::lateral(self.with_mesh_ref())
    }
}

impl<'a, G> EdgeView<&'a mut Mesh<G>, G>
where
    G: Geometry + EdgeLateral,
    G::Vertex: AsPosition,
{
    pub fn extrude<T>(self, distance: T) -> Result<EdgeView<&'a mut Mesh<G>, G>, Error>
    where
        G::Lateral: Mul<T>,
        ScaledEdgeLateral<G, T>: Clone,
        VertexPosition<G>: Add<ScaledEdgeLateral<G, T>, Output = VertexPosition<G>> + Clone,
    {
        let EdgeView { mesh, key: ab, .. } = self;
        let mut mutation = Mutation::immediate(mesh);
        let edge = extrude(&mut mutation, ab, distance)?;
        Ok(EdgeView::new(mutation.commit(), edge))
    }
}

impl<M, G> AsRef<EdgeView<M, G>> for EdgeView<M, G>
where
    M: AsRef<Mesh<G>>,
    G: Geometry,
{
    fn as_ref(&self) -> &EdgeView<M, G> {
        self
    }
}

impl<M, G> AsMut<EdgeView<M, G>> for EdgeView<M, G>
where
    M: AsRef<Mesh<G>> + AsMut<Mesh<G>>,
    G: Geometry,
{
    fn as_mut(&mut self) -> &mut EdgeView<M, G> {
        self
    }
}

impl<M, G> Deref for EdgeView<M, G>
where
    M: AsRef<Mesh<G>>,
    G: Geometry,
{
    type Target = Edge<G>;

    fn deref(&self) -> &Self::Target {
        self.mesh
            .as_ref()
            .as_storage::<Edge<G>>()
            .get(&self.key)
            .unwrap()
    }
}

impl<M, G> DerefMut for EdgeView<M, G>
where
    M: AsRef<Mesh<G>> + AsMut<Mesh<G>>,
    G: Geometry,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.mesh
            .as_mut()
            .as_storage_mut::<Edge<G>>()
            .get_mut(&self.key)
            .unwrap()
    }
}

impl<M, G> Clone for EdgeView<M, G>
where
    M: AsRef<Mesh<G>> + Clone,
    G: Geometry,
{
    fn clone(&self) -> Self {
        EdgeView {
            mesh: self.mesh.clone(),
            key: self.key,
            phantom: PhantomData,
        }
    }
}

impl<M, G> Copy for EdgeView<M, G>
where
    M: AsRef<Mesh<G>> + Copy,
    G: Geometry,
{
}

impl<M, G> View<M, G> for EdgeView<M, G>
where
    M: AsRef<Mesh<G>>,
    G: Geometry,
{
    type Topology = Edge<G>;

    fn from_mesh(mesh: M, key: <Self::Topology as Topological>::Key) -> Self {
        EdgeView::new(mesh, key)
    }
}

/// Do **not** use this type directly. Use `OrphanEdgeMut` instead.
///
/// This type is only re-exported so that its members are shown in
/// documentation. See this issue:
/// <https://github.com/rust-lang/rust/issues/39437>
pub struct OrphanEdgeView<'a, G>
where
    G: 'a + Geometry,
{
    key: EdgeKey,
    edge: &'a mut Edge<G>,
}

impl<'a, G> OrphanEdgeView<'a, G>
where
    G: 'a + Geometry,
{
    pub(in graph) fn new(edge: &'a mut Edge<G>, key: EdgeKey) -> Self {
        OrphanEdgeView {
            key: key,
            edge: edge,
        }
    }

    pub fn key(&self) -> EdgeKey {
        self.key
    }
}

impl<'a, G> Deref for OrphanEdgeView<'a, G>
where
    G: 'a + Geometry,
{
    type Target = <Self as OrphanView<'a, G>>::Topology;

    fn deref(&self) -> &Self::Target {
        &*self.edge
    }
}

impl<'a, G> DerefMut for OrphanEdgeView<'a, G>
where
    G: 'a + Geometry,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.edge
    }
}

impl<'a, G> OrphanView<'a, G> for OrphanEdgeView<'a, G>
where
    G: 'a + Geometry,
{
    type Topology = Edge<G>;

    fn from_topology(
        topology: &'a mut Self::Topology,
        key: <Self::Topology as Topological>::Key,
    ) -> Self {
        OrphanEdgeView::new(topology, key)
    }
}

pub struct VertexCirculator<M, G>
where
    M: AsRef<Mesh<G>>,
    G: Geometry,
{
    mesh: M,
    vertices: ArrayVec<[VertexKey; 2]>,
    phantom: PhantomData<G>,
}

impl<M, G> VertexCirculator<M, G>
where
    M: AsRef<Mesh<G>>,
    G: Geometry,
{
    fn new(mesh: M, vertices: ArrayVec<[VertexKey; 2]>) -> Self {
        VertexCirculator {
            mesh,
            vertices,
            phantom: PhantomData,
        }
    }
}

impl<'a, G> Iterator for VertexCirculator<&'a Mesh<G>, G>
where
    G: Geometry,
{
    type Item = VertexView<&'a Mesh<G>, G>;

    fn next(&mut self) -> Option<Self::Item> {
        self.vertices
            .pop()
            .map(|vertex| VertexView::new(self.mesh, vertex))
    }
}

impl<'a, G> Iterator for VertexCirculator<&'a mut Mesh<G>, G>
where
    G: Geometry,
{
    type Item = OrphanVertexView<'a, G>;

    fn next(&mut self) -> Option<Self::Item> {
        self.vertices.pop().map(|vertex| {
            OrphanVertexView::new(
                unsafe {
                    use std::mem;

                    // There is no way to bind the anonymous lifetime of this
                    // function to `Self::Item`. This is problematic for the
                    // call to `get_mut`, which requires autoref. However, this
                    // should be safe, because the use of this iterator
                    // requires a mutable borrow of the source mesh with
                    // lifetime `'a`. Therefore, the (disjoint) geometry data
                    // within the mesh should also be valid over the lifetime
                    // '`a'.
                    mem::transmute::<_, &'a mut Vertex<G>>(
                        self.mesh
                            .as_storage_mut::<Vertex<G>>()
                            .get_mut(&vertex)
                            .unwrap(),
                    )
                },
                vertex,
            )
        })
    }
}

pub struct FaceCirculator<M, G>
where
    M: AsRef<Mesh<G>>,
    G: Geometry,
{
    mesh: M,
    faces: ArrayVec<[FaceKey; 2]>,
    phantom: PhantomData<G>,
}

impl<M, G> FaceCirculator<M, G>
where
    M: AsRef<Mesh<G>>,
    G: Geometry,
{
    fn new(mesh: M, faces: ArrayVec<[FaceKey; 2]>) -> Self {
        FaceCirculator {
            mesh,
            faces,
            phantom: PhantomData,
        }
    }
}

impl<'a, G> Iterator for FaceCirculator<&'a Mesh<G>, G>
where
    G: Geometry,
{
    type Item = FaceView<&'a Mesh<G>, G>;

    fn next(&mut self) -> Option<Self::Item> {
        self.faces.pop().map(|face| FaceView::new(self.mesh, face))
    }
}

impl<'a, G> Iterator for FaceCirculator<&'a mut Mesh<G>, G>
where
    G: Geometry,
{
    type Item = OrphanFaceView<'a, G>;

    fn next(&mut self) -> Option<Self::Item> {
        self.faces.pop().map(|face| {
            OrphanFaceView::new(
                unsafe {
                    use std::mem;

                    // There is no way to bind the anonymous lifetime of this
                    // function to `Self::Item`. This is problematic for the
                    // call to `get_mut`, which requires autoref. However, this
                    // should be safe, because the use of this iterator
                    // requires a mutable borrow of the source mesh with
                    // lifetime `'a`. Therefore, the (disjoint) geometry data
                    // within the mesh should also be valid over the lifetime
                    // '`a'.
                    mem::transmute::<_, &'a mut Face<G>>(
                        self.mesh
                            .as_storage_mut::<Face<G>>()
                            .get_mut(&face)
                            .unwrap(),
                    )
                },
                face,
            )
        })
    }
}

#[derive(Clone, Debug)]
pub struct EdgeKeyTopology {
    key: EdgeKey,
    vertices: (VertexKey, VertexKey),
}

impl EdgeKeyTopology {
    fn new(edge: EdgeKey, vertices: (VertexKey, VertexKey)) -> Self {
        EdgeKeyTopology {
            key: edge,
            vertices: vertices,
        }
    }

    pub fn key(&self) -> EdgeKey {
        self.key
    }

    pub fn vertices(&self) -> (VertexKey, VertexKey) {
        self.vertices
    }
}

pub(in graph) fn split<'a, M, G>(mutation: &mut M, ab: EdgeKey) -> Result<VertexKey, Error>
where
    M: ModalMutation<'a, G>,
    G: 'a + EdgeMidpoint<Midpoint = VertexPosition<G>> + Geometry,
    G::Vertex: AsPosition,
{
    fn split_at_vertex<'a, M, G>(
        mutation: &mut M,
        ab: EdgeKey,
        m: VertexKey,
    ) -> Result<(EdgeKey, EdgeKey), Error>
    where
        M: ModalMutation<'a, G>,
        G: 'a + EdgeMidpoint<Midpoint = VertexPosition<G>> + Geometry,
        G::Vertex: AsPosition,
    {
        // Remove the edge and insert two truncated edges in its place.
        let (a, b) = ab.to_vertex_keys();
        let span = mutation.remove_edge(ab).unwrap();
        let am = mutation.insert_edge((a, m), span.geometry.clone())?;
        let mb = mutation.insert_edge((m, b), span.geometry.clone())?;
        // Connect the new edges to each other and their leading edges.
        {
            let mut edge = mutation.as_mesh_mut().edge_mut(am).unwrap();
            edge.next = Some(mb);
            edge.previous = span.previous;
            edge.face = span.face
        }
        {
            let mut edge = mutation.as_mesh_mut().edge_mut(mb).unwrap();
            edge.next = span.next;
            edge.previous = Some(am);
            edge.face = span.face;
        }
        if let Some(pa) = span.previous {
            mutation.as_mesh_mut().edge_mut(pa).unwrap().next = Some(am);
        }
        if let Some(bn) = span.next {
            mutation.as_mesh_mut().edge_mut(bn).unwrap().previous = Some(mb);
        }
        // Update the associated face, if any, because it may refer to the
        // removed edge.
        if let Some(face) = span.face {
            mutation.as_mesh_mut().face_mut(face).unwrap().edge = am;
        }
        Ok((am, mb))
    }

    let (ba, m) = {
        // Insert a new vertex at the midpoint.
        let (ba, midpoint) = {
            let edge = match mutation.as_mesh().edge(ab) {
                Some(edge) => edge,
                _ => return Err(GraphError::TopologyNotFound.into()),
            };
            let mut midpoint = edge.source_vertex().geometry.clone();
            *midpoint.as_position_mut() = edge.midpoint()?;
            (
                edge.raw_opposite_edge().map(|opposite| opposite.key()),
                midpoint,
            )
        };
        (ba, mutation.insert_vertex(midpoint))
    };
    // Split the half-edges. This should not fail; unwrap the results.
    split_at_vertex(mutation, ab, m).unwrap();
    if let Some(ba) = ba {
        split_at_vertex(mutation, ba, m).unwrap();
    }
    Ok(m)
}

pub(in graph) fn join<'a, M, G>(
    mutation: &mut M,
    source: EdgeKey,
    destination: EdgeKey,
) -> Result<EdgeKey, Error>
where
    M: ModalMutation<'a, G>,
    G: 'a + Geometry,
{
    match (
        mutation.as_mesh().edge(source),
        mutation.as_mesh().edge(destination),
    ) {
        (Some(_), Some(_)) => {}
        _ => return Err(GraphError::TopologyNotFound.into()),
    }
    let (a, b) = source.to_vertex_keys();
    let (c, d) = destination.to_vertex_keys();
    // At this point, we can assume the points a, b, c, and d exist in the
    // mesh. Before mutating the mesh, ensure that existing interior edges
    // are boundaries.
    for edge in [a, b, c, d]
        .perimeter()
        .flat_map(|ab| mutation.as_mesh().edge(ab.into()))
    {
        if !edge.is_boundary_edge() {
            return Err(GraphError::TopologyConflict.into());
        }
    }
    // Insert a quad joining the edges. These operations should not fail;
    // unwrap their results.
    let (edge, face) = {
        let source = mutation.as_mesh().edge(source).unwrap();
        (
            source.geometry.clone(),
            source
                .opposite_edge()
                .face()
                .map(|face| face.geometry.clone())
                .unwrap_or_else(Default::default),
        )
    };
    // TODO: Split the face to form triangles.
    mutation.insert_face(&[a, b, c, d], (edge, face)).unwrap();
    Ok(source)
}

pub(in graph) fn extrude<'a, M, G, T>(
    mutation: &mut M,
    ab: EdgeKey,
    distance: T,
) -> Result<EdgeKey, Error>
where
    M: ModalMutation<'a, G>,
    G: 'a + Geometry + EdgeLateral,
    G::Lateral: Mul<T>,
    G::Vertex: AsPosition,
    ScaledEdgeLateral<G, T>: Clone,
    VertexPosition<G>: Add<ScaledEdgeLateral<G, T>, Output = VertexPosition<G>> + Clone,
{
    // Get the extruded geometry.
    let (vertices, edge) = {
        let edge = match mutation.as_mesh().edge(ab) {
            Some(edge) => edge,
            _ => return Err(GraphError::TopologyNotFound.into()),
        };
        if !edge.is_boundary_edge() {
            return Err(GraphError::TopologyConflict.into());
        }
        let mut vertices = (
            edge.destination_vertex().geometry.clone(),
            edge.source_vertex().geometry.clone(),
        );
        let translation = edge.lateral()? * distance;
        *vertices.0.as_position_mut() = vertices.0.as_position().clone() + translation.clone();
        *vertices.1.as_position_mut() = vertices.1.as_position().clone() + translation;
        (vertices, edge.geometry.clone())
    };
    let c = mutation.insert_vertex(vertices.0);
    let d = mutation.insert_vertex(vertices.1);
    let cd = mutation.insert_edge((c, d), edge).unwrap();
    Ok(join(mutation, ab, cd).unwrap())
}

#[cfg(test)]
mod tests {
    use nalgebra::{Point2, Point3};

    use generate::*;
    use geometry::convert::IntoGeometry;
    use geometry::*;
    use graph::*;

    fn find_vertex_with_geometry<G, T>(mesh: &Mesh<G>, geometry: T) -> Option<VertexKey>
    where
        G: Geometry,
        G::Vertex: PartialEq,
        T: IntoGeometry<G::Vertex>,
    {
        let geometry = geometry.into_geometry();
        mesh.vertices()
            .find(|vertex| vertex.geometry == geometry)
            .map(|vertex| vertex.key())
    }

    fn find_edge_with_geometry<G, T>(mesh: &Mesh<G>, geometry: (T, T)) -> Option<EdgeKey>
    where
        G: Geometry,
        G::Vertex: PartialEq,
        T: IntoGeometry<G::Vertex>,
    {
        let (source, destination) = geometry;
        match (
            find_vertex_with_geometry(mesh, source),
            find_vertex_with_geometry(mesh, destination),
        ) {
            (Some(source), Some(destination)) => Some((source, destination).into()),
            _ => None,
        }
    }

    #[test]
    fn extrude_edge() {
        let mut mesh = Mesh::<Point2<f32>>::from_raw_buffers(
            vec![0, 1, 2, 3],
            vec![(0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0)],
            4,
        ).unwrap();
        let source = find_edge_with_geometry(&mesh, ((1.0, 1.0), (1.0, 0.0))).unwrap();
        mesh.edge_mut(source).unwrap().extrude(1.0).unwrap();

        assert_eq!(14, mesh.edge_count());
        assert_eq!(2, mesh.face_count());
    }

    #[test]
    fn join_edges() {
        // Construct a mesh with two independent quads.
        let mut mesh = Mesh::<Point3<f32>>::from_raw_buffers(
            vec![0, 1, 2, 3, 4, 5, 6, 7],
            vec![
                (-2.0, 0.0, 0.0),
                (-1.0, 0.0, 0.0), // 1
                (-1.0, 1.0, 0.0), // 2
                (-2.0, 1.0, 0.0),
                (1.0, 0.0, 0.0), // 4
                (2.0, 0.0, 0.0),
                (2.0, 1.0, 0.0),
                (1.0, 1.0, 0.0), // 7
            ],
            4,
        ).unwrap();
        let source = find_edge_with_geometry(&mesh, ((-1.0, 1.0, 0.0), (-1.0, 0.0, 0.0))).unwrap();
        let destination =
            find_edge_with_geometry(&mesh, ((1.0, 0.0, 0.0), (1.0, 1.0, 0.0))).unwrap();
        mesh.edge_mut(source).unwrap().join(destination).unwrap();

        assert_eq!(20, mesh.edge_count());
        assert_eq!(3, mesh.face_count());
    }

    #[test]
    fn split_composite_edge() {
        let (indeces, vertices) = cube::Cube::new()
            .polygons_with_position() // 6 quads, 24 vertices.
            .flat_index_vertices(HashIndexer::default());
        let mut mesh = Mesh::<Point3<f32>>::from_raw_buffers(indeces, vertices, 4).unwrap();
        let key = mesh.edges().nth(0).unwrap().key();
        let vertex = mesh.edge_mut(key).unwrap().split().unwrap();

        assert_eq!(5, vertex.outgoing_edge().face().unwrap().edges().count());
        assert_eq!(
            5,
            vertex
                .outgoing_edge()
                .opposite_edge()
                .face()
                .unwrap()
                .edges()
                .count()
        );
    }
}
