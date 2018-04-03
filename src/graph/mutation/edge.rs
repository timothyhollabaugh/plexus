use failure::Error;
use std::collections::HashSet;
use std::ops::{Deref, DerefMut};

use geometry::Geometry;
use graph::{GraphError, ResultExt};
use graph::mesh::{Edge, Inconsistent, Mesh};
use graph::mutation::{Commit, Mode, Mutate};
use graph::mutation::vertex::VertexMutation;
use graph::storage::{EdgeKey, VertexKey};

pub struct EdgeMutation<G>
where
    G: Geometry,
{
    mutation: VertexMutation<G>,
    touched: HashSet<EdgeKey>,
}

impl<G> EdgeMutation<G>
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
        if self.mutant().edges.contains_key(&ab) {
            return Err(GraphError::TopologyConflict.into());
        }
        if !self.mutant().vertices.contains_key(&b) {
            return Err(GraphError::TopologyNotFound.into());
        }
        let mut edge = Edge::new(b, geometry);
        if let Some(opposite) = self.mutant_mut().edges.get_mut(&ba) {
            edge.opposite = Some(ba);
            opposite.opposite = Some(ab);
        }
        self.mutant_mut().edges.insert_with_key(&ab, edge);
        self.connect_outgoing_edge(a, ab)?;
        Ok(ab)
    }

    pub fn get_or_insert_edge(
        &mut self,
        vertices: (VertexKey, VertexKey),
        geometry: G::Edge,
    ) -> Result<EdgeKey, Error> {
        self.insert_edge(vertices, geometry)
            .or_if_conflict(|| Ok(vertices.into()))
    }

    pub fn get_or_insert_composite_edge(
        &mut self,
        vertices: (VertexKey, VertexKey),
        geometry: G::Edge,
    ) -> Result<(EdgeKey, EdgeKey), Error> {
        let (a, b) = vertices;
        let ab = self.get_or_insert_edge((a, b), geometry.clone())?;
        let ba = self.get_or_insert_edge((b, a), geometry)?;
        Ok((ab, ba))
    }

    pub fn remove_edge(&mut self, ab: EdgeKey) -> Result<Edge<G>, Error> {
        let (a, _) = ab.to_vertex_keys();
        self.disconnect_next_edge(ab).map_conflict_to_ok()?;
        self.disconnect_previous_edge(ab).map_conflict_to_ok()?;
        self.disconnect_outgoing_edge(a)
            .map_conflict_to_ok()?;
        Ok(self.mutant_mut().edges.remove(&ab).unwrap())
    }

    pub fn remove_composite_edge(&mut self, ab: EdgeKey) -> Result<(Edge<G>, Edge<G>), Error> {
        let (a, b) = ab.to_vertex_keys();
        let edge = self.remove_edge((a, b).into())?;
        let opposite = self.remove_edge((b, a).into())?;
        Ok((edge, opposite))
    }

    pub fn connect_neighbor_edges(&mut self, ab: EdgeKey, bc: EdgeKey) -> Result<(), Error> {
        if ab.to_vertex_keys().1 == bc.to_vertex_keys().0 {
            let previous = self.mutant_mut().edge_mut(ab);
            let next = self.mutant_mut().edge_mut(bc);
            match (previous, next) {
                (Some(previous), Some(next)) => match (previous.next, next.previous) {
                    (None, None) => {
                        previous.next = Some(bc);
                        next.previous = Some(ab);
                        Ok(())
                    }
                    _ => Err(Error::from(GraphError::TopologyConflict)),
                },
                _ => Err(Error::from(GraphError::TopologyNotFound)),
            }
        }
        else {
            Err(Error::from(GraphError::TopologyMalformed))
        }
    }

    pub fn disconnect_next_edge(&mut self, ab: EdgeKey) -> Result<EdgeKey, Error> {
        let mut edge = self.mutant_mut()
            .edge_mut(ab)
            .ok_or_else(|| Error::from(GraphError::TopologyNotFound))?;
        if let Some(bx) = edge.next.take() {
            self.mutant_mut().edge_mut(bx).unwrap().previous = None;
            Ok(bx)
        }
        else {
            Err(Error::from(GraphError::TopologyConflict))
        }
    }

    pub fn disconnect_previous_edge(&mut self, ab: EdgeKey) -> Result<EdgeKey, Error> {
        let mut edge = self.mutant_mut()
            .edge_mut(ab)
            .ok_or_else(|| Error::from(GraphError::TopologyNotFound))?;
        if let Some(xa) = edge.previous.take() {
            self.mutant_mut().edge_mut(xa).unwrap().previous = None;
            Ok(xa)
        }
        else {
            Err(Error::from(GraphError::TopologyConflict))
        }
    }
}

impl<G> Commit<G> for EdgeMutation<G>
where
    G: Geometry,
{
    type Error = Error;

    fn commit(self) -> Result<Self::Mutant, Self::Error> {
        let EdgeMutation { mutation, touched } = self;
        mutation.commit().and_then(|mutant| {
            for edge in touched {}
            Ok(mutant)
        })
    }
}

impl<G> Deref for EdgeMutation<G>
where
    G: Geometry,
{
    type Target = VertexMutation<G>;

    fn deref(&self) -> &Self::Target {
        &self.mutation
    }
}

impl<G> DerefMut for EdgeMutation<G>
where
    G: Geometry,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.mutation
    }
}

impl<G> Mode<G> for EdgeMutation<G>
where
    G: Geometry,
{
    type Mutant = Mesh<G, Inconsistent>;
}

impl<G> Mutate<G> for EdgeMutation<G>
where
    G: Geometry,
{
    fn mutate(mutant: Self::Mutant) -> Self {
        EdgeMutation {
            touched: Default::default(),
            mutation: VertexMutation::mutate(mutant),
        }
    }
}
