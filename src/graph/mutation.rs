use failure::{Error, Fail};
use std::collections::{HashMap, HashSet};
use std::mem;
use std::ops::{Deref, DerefMut};

use geometry::Geometry;
use graph::{GraphError, Perimeter};
use graph::mesh::{Connectivity, Consistent, Edge, Face, Inconsistent, Mesh, Vertex};
use graph::storage::{EdgeKey, FaceKey, VertexKey};

trait Mutate<G>
where
    G: Geometry,
{
    fn mutate(mesh: Mesh<G, Consistent>) -> Self;
}

trait Commit<G>
where
    G: Geometry,
{
    fn commit(self) -> Result<Mesh<G, Consistent>, Error>;
}

pub trait ModalMutation<G>: Deref<Target = Mutation<G>> + DerefMut
where
    G: Geometry,
{
    fn insert_face(
        &mut self,
        vertices: &[VertexKey],
        geometry: (G::Edge, G::Face),
    ) -> Result<FaceKey, Error>;

    fn remove_face(&mut self, face: FaceKey) -> Result<Face<G>, Error>;
}

/// Mesh mutation.
///
/// Mutates a `Mesh`. This type provides general operations that are supported
/// by both immediate and batch mutations.
pub struct Mutation<G>
where
    G: Geometry,
{
    mesh: Mesh<G, Inconsistent>,
}

impl<G> Mutation<G>
where
    G: Geometry,
{
    fn mutate(mesh: Mesh<G, Consistent>) -> Self {
        Mutation {
            mesh: mesh.into_consistency(),
        }
    }

    pub fn as_mesh(&self) -> &Mesh<G, Inconsistent> {
        &self.mesh
    }

    pub fn as_mesh_mut(&mut self) -> &mut Mesh<G, Inconsistent> {
        &mut self.mesh
    }
}

/// Vertex mutations.
impl<G> Mutation<G>
where
    G: Geometry,
{
    pub fn insert_vertex(&mut self, geometry: G::Vertex) -> VertexKey {
        self.mesh
            .vertices
            .insert_with_generator(Vertex::new(geometry))
    }
}

/// Edge mutations.
impl<G> Mutation<G>
where
    G: Geometry,
{
    pub fn insert_edge(
        &mut self,
        vertices: (VertexKey, VertexKey),
        geometry: G::Edge,
    ) -> Result<EdgeKey, Error> {
        let (a, b) = vertices;
        let ab = (a, b).into();
        let ba = (b, a).into();
        // If the edge already exists, then fail. This ensures an important
        // invariant: edges may only have two adjacent faces. That is, a
        // half-edge may only have one associated face, at most one preceding
        // half-edge, at most one following half-edge, and may form at most one
        // closed loop.
        if self.mesh.edges.contains_key(&ab) {
            return Err(GraphError::TopologyConflict.into());
        }
        let vertex = {
            if !self.mesh.vertices.contains_key(&b) {
                return Err(GraphError::TopologyNotFound.into());
            }
            match self.mesh.vertices.get_mut(&a) {
                Some(vertex) => vertex,
                _ => {
                    return Err(GraphError::TopologyNotFound.into());
                }
            }
        };
        let mut edge = Edge::new(b, geometry);
        // This is the point of no return. The mesh has been mutated. Unwrap
        // results.
        if let Some(opposite) = self.mesh.edges.get_mut(&ba) {
            edge.opposite = Some(ba);
            opposite.opposite = Some(ab);
        }
        self.mesh.edges.insert_with_key(&ab, edge);
        vertex.edge = Some(ab);
        Ok(ab)
    }

    fn get_or_insert_edge(
        &mut self,
        vertices: (VertexKey, VertexKey),
        geometry: G::Edge,
    ) -> Result<EdgeKey, Error> {
        self.insert_edge(vertices, geometry).or_else(|error| {
            match error.downcast::<GraphError>().unwrap() {
                GraphError::TopologyConflict => Ok(vertices.into()),
                error => Err(error.into()),
            }
        })
    }

    fn get_or_insert_composite_edge(
        &mut self,
        vertices: (VertexKey, VertexKey),
        geometry: G::Edge,
    ) -> Result<(EdgeKey, EdgeKey), Error> {
        let (a, b) = vertices;
        let ab = self.get_or_insert_edge((a, b), geometry.clone())?;
        let ba = self.get_or_insert_edge((b, a), geometry)?;
        Ok((ab, ba))
    }

    pub fn remove_edge(&mut self, edge: EdgeKey) -> Result<Edge<G>, Error> {
        if let Some(mut edge) = self.mesh.edge_mut(edge) {
            if let Some(mut next) = edge.next_edge_mut() {
                next.previous = None;
            }
            if let Some(mut previous) = edge.previous_edge_mut() {
                previous.next = None;
            }
            edge.source_vertex_mut().edge = None;
        }
        else {
            return Err(Error::from(GraphError::TopologyNotFound));
        }
        Ok(self.mesh.edges.remove(&edge).unwrap())
    }

    fn remove_composite_edge(&mut self, edge: EdgeKey) -> Result<(Edge<G>, Edge<G>), Error> {
        let (a, b) = edge.to_vertex_keys();
        let edge = self.remove_edge((a, b).into())?;
        let opposite = self.remove_edge((b, a).into())?;
        Ok((edge, opposite))
    }
}

/// Face mutations.
impl<G> Mutation<G>
where
    G: Geometry,
{
    fn connect_face_interior(&mut self, edges: &[EdgeKey], face: FaceKey) -> Result<(), Error> {
        for (ab, bc) in edges.perimeter() {
            {
                let mut edge = self.mesh.edge_mut(ab).unwrap();
                edge.next = Some(bc);
                edge.face = Some(face);
            }
            {
                let mut edge = self.mesh.edge_mut(bc).unwrap();
                edge.previous = Some(ab);
            }
        }
        Ok(())
    }

    fn connect_face_exterior(
        &mut self,
        edges: &[EdgeKey],
        connectivity: (Connectivity, Connectivity),
    ) -> Result<(), Error> {
        let (incoming, outgoing) = connectivity;
        for (a, b) in edges.iter().map(|edge| edge.to_vertex_keys()) {
            // Only boundary edges must be connected.
            if self.mesh.edge((b, a).into()).unwrap().face().is_none() {
                // The next edge of b-a is the outgoing edge of the destination
                // vertex A that is also a boundary edge or, if there is no
                // such outgoing edge, the next exterior edge of the face. The
                // previous edge is similar.
                let ax = outgoing[&a]
                    .iter()
                    .map(|ax| self.mesh.edge(*ax).unwrap())
                    .find(|edge| edge.face().is_none())
                    .or_else(|| {
                        self.mesh
                            .edge((a, b).into())
                            .unwrap()
                            .into_previous_edge()
                            .unwrap()
                            .into_opposite_edge()
                    })
                    .unwrap()
                    .key();
                {
                    self.mesh.edge_mut((b, a).into()).unwrap().next = Some(ax);
                    self.mesh.edge_mut(ax).unwrap().previous = Some((b, a).into());
                }
                let xb = incoming[&b]
                    .iter()
                    .map(|xb| self.mesh.edge(*xb).unwrap())
                    .find(|edge| edge.face().is_none())
                    .or_else(|| {
                        self.mesh
                            .edge((a, b).into())
                            .unwrap()
                            .into_next_edge()
                            .unwrap()
                            .into_opposite_edge()
                    })
                    .unwrap()
                    .key();
                {
                    self.mesh.edge_mut((b, a).into()).unwrap().previous = Some(xb);
                    self.mesh.edge_mut(xb).unwrap().next = Some((b, a).into());
                }
            }
        }
        Ok(())
    }

    fn disconnect_face_interior(&mut self, edges: &[EdgeKey]) -> Result<(), Error> {
        for ab in edges {
            self.mesh
                .edge_mut(*ab)
                .ok_or_else(|| Error::from(GraphError::TopologyNotFound))?
                .face = None;
        }
        Ok(())
    }
}

/// Immediate mesh mutations.
///
/// This type provides mutations that are only available in immediate mode.
/// Immediate mutations are atomic, and fail immediately if the integrity of
/// the mesh is compromised.
pub struct ImmediateMutation<G>
where
    G: Geometry,
{
    mutation: Mutation<G>,
}

impl<G> ImmediateMutation<G>
where
    G: Geometry,
{
    pub fn mutate(mesh: Mesh<G, Consistent>) -> Self {
        ImmediateMutation {
            mutation: Mutation::mutate(mesh),
        }
    }

    pub fn replace(
        mesh: &mut Mesh<G, Consistent>,
        replacement: Mesh<G, Consistent>,
    ) -> ReplaceMutation<Self, G> {
        ReplaceMutation::mutate(mesh, replacement)
    }

    pub fn commit(self) -> Mesh<G, Consistent> {
        self.mutation.mesh.into_consistency()
    }
}

impl<G> Mutate<G> for ImmediateMutation<G>
where
    G: Geometry,
{
    fn mutate(mesh: Mesh<G, Consistent>) -> Self {
        ImmediateMutation::mutate(mesh)
    }
}

impl<G> Commit<G> for ImmediateMutation<G>
where
    G: Geometry,
{
    fn commit(self) -> Result<Mesh<G, Consistent>, Error> {
        Ok(ImmediateMutation::<G>::commit(self))
    }
}

impl<G> ModalMutation<G> for ImmediateMutation<G>
where
    G: Geometry,
{
    fn insert_face(
        &mut self,
        vertices: &[VertexKey],
        geometry: (G::Edge, G::Face),
    ) -> Result<FaceKey, Error> {
        // Before mutating the mesh, verify that the region is not already
        // occupied by a face and collect the incoming and outgoing edges for
        // each vertex in the region.
        let region = self.mesh.region(vertices)?;
        if region.face().is_some() {
            return Err(GraphError::TopologyConflict.into());
        }
        let ((incoming, outgoing), singularity) = self.mesh.reachable_region_connectivity(region);
        if singularity.is_some() {
            return Err(GraphError::TopologyMalformed
                .context("non-manifold connectivity")
                .into());
        }
        // Insert composite edges and collect the interior edges. This is the
        // point of no return; the mesh has been mutated. Unwrap results.
        let edges = vertices
            .perimeter()
            .map(|ab| {
                self.get_or_insert_composite_edge(ab, geometry.0.clone())
                    .unwrap()
                    .0
            })
            .collect::<Vec<_>>();
        let face = self.mesh
            .faces
            .insert_with_generator(Face::new(edges[0], geometry.1));
        self.connect_face_interior(&edges, face).unwrap();
        self.connect_face_exterior(&edges, (incoming, outgoing))
            .unwrap();
        Ok(face)
    }

    fn remove_face(&mut self, face: FaceKey) -> Result<Face<G>, Error> {
        let edges = {
            let face = match self.mesh.face(face) {
                Some(face) => face,
                _ => return Err(GraphError::TopologyNotFound.into()),
            };
            // Iterate over the set of vertices shared between the face and all
            // of its neighbors. These are potential singularities.
            for vertex in face.mutuals() {
                // Circulate (in order) over the neighboring faces of the
                // potential singularity, ignoring the face to be removed.
                // Count the number of gaps, where neighboring faces do not
                // share any edges. Because a face is being ignored, exactly
                // one gap is expected. If any additional gaps exist, then
                // removal will create a singularity.
                let vertex = self.mesh.vertex(vertex).unwrap();
                let n = vertex
                    .faces()
                    .filter(|neighbor| neighbor.key() != face.key())
                    .collect::<Vec<_>>()
                    .iter()
                    .perimeter()
                    .filter(|&(previous, next)| {
                        let exterior = previous
                            .edges()
                            .map(|edge| edge.into_opposite_edge())
                            .map(|edge| edge.key())
                            .collect::<HashSet<_>>();
                        let interior = next.edges().map(|edge| edge.key()).collect::<HashSet<_>>();
                        exterior.intersection(&interior).count() == 0
                    })
                    .count();
                if n > 1 {
                    return Err(GraphError::TopologyConflict.into());
                }
            }
            // Find any boundary edges. Once this face is removed, such edges
            // will have no face on either side.
            face.interior_edges()
                .flat_map(|edge| edge.into_boundary_edge())
                .map(|edge| edge.key())
                .collect::<Vec<_>>()
        };
        self.disconnect_face_interior(face)?;
        for edge in edges {
            self.remove_composite_edge(edge).unwrap();
        }
        Ok(self.mesh.faces.remove(&face).unwrap())
    }
}

impl<G> Deref for ImmediateMutation<G>
where
    G: Geometry,
{
    type Target = Mutation<G>;

    fn deref(&self) -> &Self::Target {
        &self.mutation
    }
}

impl<G> DerefMut for ImmediateMutation<G>
where
    G: Geometry,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.mutation
    }
}

/// Batch mesh mutations.
///
/// This type provides mutations that are only available in batch mode. Batch
/// mutations defer certain types of integrity errors until a series of
/// mutations are complete. When `commit` is called, the integrity of the mesh
/// is verified before yielding it to the caller.
pub struct BatchMutation<G>
where
    G: Geometry,
{
    mutation: Mutation<G>,
    singularities: HashMap<VertexKey, HashSet<FaceKey>>,
}

impl<G> BatchMutation<G>
where
    G: Geometry,
{
    pub fn mutate(mesh: Mesh<G, Consistent>) -> Self {
        BatchMutation {
            mutation: Mutation::mutate(mesh),
            singularities: HashMap::new(),
        }
    }

    pub fn replace(
        mesh: &mut Mesh<G, Consistent>,
        replacement: Mesh<G, Consistent>,
    ) -> ReplaceMutation<Self, G> {
        ReplaceMutation::mutate(mesh, replacement)
    }

    pub fn commit(self) -> Result<Mesh<G, Consistent>, Error> {
        let mesh = self.mutation.mesh.into_consistency();
        for (vertex, faces) in self.singularities {
            // TODO: This will not detect exactly two faces joined by a single
            //       vertex. This is technically supported, but perhaps should
            //       be rejected.
            // Determine if any unreachable faces exist in the mesh. This
            // cannot happen if the mesh is ultimately a manifold and edge
            // connectivity heals.
            if let Some(vertex) = mesh.vertex(vertex) {
                for unreachable in
                    faces.difference(&vertex.faces().map(|face| face.key()).collect())
                {
                    if mesh.face(*unreachable).is_some() {
                        return Err(GraphError::TopologyMalformed
                            .context("non-manifold connectivity")
                            .into());
                    }
                }
            }
        }
        Ok(mesh)
    }
}

impl<G> Mutate<G> for BatchMutation<G>
where
    G: Geometry,
{
    fn mutate(mesh: Mesh<G, Consistent>) -> Self {
        BatchMutation::mutate(mesh)
    }
}

impl<G> Commit<G> for BatchMutation<G>
where
    G: Geometry,
{
    fn commit(self) -> Result<Mesh<G, Consistent>, Error> {
        BatchMutation::<G>::commit(self)
    }
}

impl<G> ModalMutation<G> for BatchMutation<G>
where
    G: Geometry,
{
    fn insert_face(
        &mut self,
        vertices: &[VertexKey],
        geometry: (G::Edge, G::Face),
    ) -> Result<FaceKey, Error> {
        // Before mutating the mesh, verify that the region is not already
        // occupied by a face and collect the incoming and outgoing edges for
        // each vertex in the region.
        let region = self.mesh.region(vertices)?;
        if region.face().is_some() {
            return Err(GraphError::TopologyConflict.into());
        }
        let ((incoming, outgoing), singularity) = self.mesh.reachable_region_connectivity(region);
        // Insert composite edges and collect the interior edges. This is the
        // point of no return; the mesh has been mutated. Unwrap results.
        let edges = vertices
            .perimeter()
            .map(|ab| {
                self.get_or_insert_composite_edge(ab, geometry.0.clone())
                    .unwrap()
                    .0
            })
            .collect::<Vec<_>>();
        let face = self.mesh
            .faces
            .insert_with_generator(Face::new(edges[0], geometry.1));
        if let Some(singularity) = singularity {
            let faces = self.singularities
                .entry(singularity.0)
                .or_insert_with(Default::default);
            for face in singularity.1 {
                faces.insert(face);
            }
            faces.insert(face);
        }
        self.connect_face_interior(&edges, face).unwrap();
        self.connect_face_exterior(&edges, (incoming, outgoing))
            .unwrap();
        Ok(face)
    }

    // TODO: This should participate in consistency checks. Removing a face
    //       could, for example, lead to singularities.
    fn remove_face(&mut self, face: FaceKey) -> Result<Face<G>, Error> {
        let edges = {
            let face = match self.mesh.face(face) {
                Some(face) => face,
                _ => return Err(GraphError::TopologyNotFound.into()),
            };
            // Find any boundary edges. Once this face is removed, such edges
            // will have no face on either side.
            face.interior_edges()
                .flat_map(|edge| edge.into_boundary_edge())
                .map(|edge| edge.key())
                .collect::<Vec<_>>()
        };
        self.disconnect_face_interior(face)?;
        for edge in edges {
            self.remove_composite_edge(edge).unwrap();
        }
        Ok(self.mesh.faces.remove(&face).unwrap())
    }
}

impl<G> Deref for BatchMutation<G>
where
    G: Geometry,
{
    type Target = Mutation<G>;

    fn deref(&self) -> &Self::Target {
        &self.mutation
    }
}

impl<G> DerefMut for BatchMutation<G>
where
    G: Geometry,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.mutation
    }
}

pub struct ReplaceMutation<'a, M, G>
where
    M: ModalMutation<G>,
    G: 'a + Geometry,
{
    mesh: &'a mut Mesh<G, Consistent>,
    mutation: M,
}

impl<'a, M, G> ReplaceMutation<'a, M, G>
where
    M: Commit<G> + ModalMutation<G> + Mutate<G>,
    G: 'a + Geometry,
{
    fn mutate(mesh: &'a mut Mesh<G, Consistent>, replacement: Mesh<G, Consistent>) -> Self {
        let mutant = mem::replace(mesh, replacement);
        ReplaceMutation {
            mesh: mesh,
            mutation: M::mutate(mutant),
        }
    }

    pub fn commit(self) -> Result<&'a mut Mesh<G, Consistent>, Error> {
        let ReplaceMutation { mesh, mutation } = self;
        mem::replace(mesh, mutation.commit()?);
        Ok(mesh)
    }
}

impl<'a, M, G> Deref for ReplaceMutation<'a, M, G>
where
    M: ModalMutation<G>,
    G: 'a + Geometry,
{
    type Target = M;

    fn deref(&self) -> &Self::Target {
        &self.mutation
    }
}

impl<'a, M, G> DerefMut for ReplaceMutation<'a, M, G>
where
    M: ModalMutation<G>,
    G: 'a + Geometry,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.mutation
    }
}
