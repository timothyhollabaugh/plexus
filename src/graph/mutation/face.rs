use failure::{Error, Fail};
use std::collections::{HashMap, HashSet};
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};

use geometry::Geometry;
use graph::mesh::{Connectivity, Consistency, Consistent, Face, Inconsistent, Mesh, Region,
                  Singularity};
use graph::mutation::edge::EdgeMutation;
use graph::mutation::{Commit, Mode, Mutate};
use graph::storage::{EdgeKey, FaceKey, VertexKey};
use graph::{GraphError, Perimeter};

pub struct FaceMutation<G>
where
    G: Geometry,
{
    mutation: EdgeMutation<G>,
    touched: HashSet<FaceKey>,
    singularities: HashMap<VertexKey, HashSet<FaceKey>>,
}

impl<G> FaceMutation<G>
where
    G: Geometry,
{
    pub fn insert_face(
        &mut self,
        vertices: &[VertexKey],
        geometry: (G::Edge, G::Face),
    ) -> Result<FaceKey, Error> {
        let cache = FaceInsertCache::snapshot(self.mutant(), vertices, geometry)?;
        self.insert_face_with_cache(cache)
    }

    pub fn insert_face_with_cache<C>(
        &mut self,
        cache: FaceInsertCache<G, C>,
    ) -> Result<FaceKey, Error>
    where
        C: Consistency,
    {
        let FaceInsertCache {
            region,
            connectivity,
            singularity,
            geometry,
            ..
        } = cache;
        // Insert composite edges and collect the interior edges.
        let edges = region
            .vertices()
            .perimeter()
            .map(|ab| {
                self.get_or_insert_composite_edge(ab, geometry.0.clone())
                    // TODO: Do not use `unwrap`. In the worst case, unroll the
                    //       iterator expression.
                    .unwrap()
                    .0
            })
            .collect::<Vec<_>>();
        // Insert the face and add it to the set of touched faces.
        let face = self.mutant_mut()
            .faces
            .insert(Face::new(edges[0], geometry.1));
        self.touched.insert(face);
        // If a singularity was detected, record it and its neighboring faces.
        if let Some(singularity) = singularity {
            let faces = self.singularities
                .entry(singularity.0)
                .or_insert_with(Default::default);
            for face in singularity.1 {
                faces.insert(face);
            }
            faces.insert(face);
        }
        self.connect_face_interior(&edges, face)?;
        self.connect_face_exterior(&edges, connectivity)?;
        Ok(face)
    }

    pub fn remove_face_with_cache<C>(
        &mut self,
        cache: FaceRemoveCache<G, C>,
    ) -> Result<Face<G>, Error>
    where
        C: Consistency,
    {
        let FaceRemoveCache {
            abc,
            mutuals,
            edges,
            boundaries,
            ..
        } = cache;
        // Iterate over the set of vertices shared between the face and all
        // of its neighbors. These are potential singularities.
        for vertex in mutuals {
            // Circulate (in order) over the neighboring faces of the
            // potential singularity, ignoring the face to be removed.
            // Count the number of gaps, where neighboring faces do not
            // share any edges. Because a face is being ignored, exactly
            // one gap is expected. If any additional gaps exist, then
            // removal will create a singularity.
            let vertex = self.mutant()
                .vertex(vertex)
                .ok_or_else(|| GraphError::TopologyNotFound.into())?;
            let n = vertex
                .faces()
                .filter(|face| face.key() != abc)
                .collect::<Vec<_>>()
                .iter()
                .perimeter()
                .filter(|&(previous, next)| {
                    let exterior = previous
                        .interior_edges()
                        .map(|edge| edge.into_opposite_edge())
                        .map(|edge| edge.key())
                        .collect::<HashSet<_>>();
                    let interior = next.interior_edges()
                        .map(|edge| edge.key())
                        .collect::<HashSet<_>>();
                    exterior.intersection(&interior).count() == 0
                })
                .count();
            if n > 1 {
                return Err(GraphError::TopologyConflict.into());
            }
        }
        self.disconnect_face_interior(&edges)?;
        for ab in boundaries {
            self.remove_composite_edge(ab)?;
        }
        let face = self.mutant_mut()
            .faces
            .remove(&abc)
            .ok_or_else(|| GraphError::TopologyNotFound.into())?;
        self.touched.insert(abc);
        Ok(face)
    }

    fn connect_face_interior(&mut self, edges: &[EdgeKey], face: FaceKey) -> Result<(), Error> {
        for (ab, bc) in edges.perimeter() {
            {
                let mut edge = self.mutant_mut().edge_mut(ab).unwrap();
                edge.next = Some(bc);
                edge.face = Some(face);
            }
            {
                let mut edge = self.mutant_mut().edge_mut(bc).unwrap();
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
            if self.mutant().edge((b, a).into()).unwrap().face().is_none() {
                // The next edge of B-A is the outgoing edge of the destination
                // vertex A that is also a boundary edge or, if there is no
                // such outgoing edge, the next exterior edge of the face. The
                // previous edge is similar.
                let ax = outgoing[&a]
                    .iter()
                    .map(|ax| self.mutant().edge(*ax).unwrap())
                    .find(|edge| edge.face().is_none())
                    .or_else(|| {
                        self.mutant()
                            .edge((a, b).into())
                            .unwrap()
                            .into_previous_edge()
                            .unwrap()
                            .into_opposite_edge()
                    })
                    .unwrap()
                    .key();
                let xb = incoming[&b]
                    .iter()
                    .map(|xb| self.mutant().edge(*xb).unwrap())
                    .find(|edge| edge.face().is_none())
                    .or_else(|| {
                        self.mutant()
                            .edge((a, b).into())
                            .unwrap()
                            .into_next_edge()
                            .unwrap()
                            .into_opposite_edge()
                    })
                    .unwrap()
                    .key();
                self.connect_neighbor_edges((b, a).into(), ax)?;
                self.connect_neighbor_edges(xb, (b, a).into())?;
            }
        }
        Ok(())
    }

    fn disconnect_face_interior(&mut self, edges: &[EdgeKey]) -> Result<(), Error> {
        for ab in edges {
            self.mutant_mut()
                .edge_mut(*ab)
                .ok_or_else(|| Error::from(GraphError::TopologyNotFound))?
                .face = None;
        }
        Ok(())
    }
}

impl<G> Commit<G> for FaceMutation<G>
where
    G: Geometry,
{
    type Error = Error;

    fn commit(self) -> Result<Self::Mutant, Self::Error> {
        let FaceMutation {
            mutation,
            touched,
            singularities,
        } = self;
        mutation.commit().and_then(|mutant| {
            for face in touched {}
            for (vertex, faces) in singularities {
                // TODO: This will not detect exactly two faces joined by a single
                //       vertex. This is technically supported, but perhaps should
                //       be detected and rejected.
                // Determine if any unreachable faces exist in the mesh. This
                // cannot happen if the mesh is ultimately a manifold and edge
                // connectivity heals.
                if let Some(vertex) = mutant.vertex(vertex) {
                    for unreachable in
                        faces.difference(&vertex.reachable_faces().map(|face| face.key()).collect())
                    {
                        if mutant.face(*unreachable).is_some() {
                            return Err(GraphError::TopologyMalformed
                                .context("non-manifold connectivity")
                                .into());
                        }
                    }
                }
            }
            Ok(mutant)
        })
    }
}

impl<G> Deref for FaceMutation<G>
where
    G: Geometry,
{
    type Target = EdgeMutation<G>;

    fn deref(&self) -> &Self::Target {
        &self.mutation
    }
}

impl<G> DerefMut for FaceMutation<G>
where
    G: Geometry,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.mutation
    }
}

impl<G> Mode<G> for FaceMutation<G>
where
    G: Geometry,
{
    type Mutant = Mesh<G, Inconsistent>;
}

impl<G> Mutate<G> for FaceMutation<G>
where
    G: Geometry,
{
    fn mutate(mutant: Self::Mutant) -> Self {
        FaceMutation {
            touched: Default::default(),
            singularities: Default::default(),
            mutation: EdgeMutation::mutate(mutant),
        }
    }
}
pub struct FaceInsertCache<'a, G, C>
where
    G: Geometry,
    C: Consistency,
{
    region: Region<'a>,
    connectivity: (Connectivity, Connectivity),
    singularity: Option<Singularity>,
    geometry: (G::Edge, G::Face),
    phantom: PhantomData<C>,
}

impl<'a, G, C> FaceInsertCache<'a, G, C>
where
    G: Geometry,
    C: Consistency,
{
    pub fn snapshot(
        mesh: &Mesh<G, C>,
        vertices: &'a [VertexKey],
        geometry: (G::Edge, G::Face),
    ) -> Result<Self, Error> {
        // Verify that the region is not already occupied by a face and collect
        // the incoming and outgoing edges for each vertex in the region.
        let region = mesh.region(vertices)?;
        if region.face().is_some() {
            return Err(GraphError::TopologyConflict.into());
        }
        let (connectivity, singularity) = mesh.reachable_region_connectivity(region);
        Ok(FaceInsertCache {
            region,
            connectivity,
            singularity,
            geometry,
            phantom: PhantomData,
        })
    }
}

pub struct FaceRemoveCache<G, C>
where
    G: Geometry,
    C: Consistency,
{
    abc: FaceKey,
    mutuals: Vec<VertexKey>,
    edges: Vec<EdgeKey>,
    boundaries: Vec<EdgeKey>,
    phantom: PhantomData<(G, C)>,
}

impl<G> FaceRemoveCache<G, Consistent>
where
    G: Geometry,
{
    pub fn snapshot(mesh: &Mesh<G, Consistent>, abc: FaceKey) -> Result<Self, Error> {
        let face = match mesh.face(abc) {
            Some(face) => face,
            _ => return Err(GraphError::TopologyNotFound.into()),
        };
        Ok(FaceRemoveCache {
            abc,
            mutuals: face.mutuals().into_iter().collect(),
            edges: face.interior_edges().map(|edge| edge.key()).collect(),
            // Find any boundary edges. Once this face is removed, such edges
            // will have no face on either side.
            boundaries: face.interior_edges()
                .flat_map(|edge| edge.into_boundary_edge())
                .map(|edge| edge.key())
                .collect(),
            phantom: PhantomData,
        })
    }
}
