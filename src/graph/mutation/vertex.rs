use failure::Error;
use std::collections::HashSet;

use geometry::Geometry;
use graph::mesh::{Inconsistent, Mesh, Vertex};
use graph::mutation::{Commit, Mode, Mutate};
use graph::storage::{EdgeKey, VertexKey};
use graph::GraphError;

pub struct VertexMutation<G>
where
    G: Geometry,
{
    mutant: Mesh<G, Inconsistent>,
    touched: HashSet<VertexKey>,
}

impl<G> VertexMutation<G>
where
    G: Geometry,
{
    pub fn mutant(&self) -> &Mesh<G, Inconsistent> {
        &self.mutant
    }

    pub(in graph::mutation) fn mutant_mut(&mut self) -> &mut Mesh<G, Inconsistent> {
        &mut self.mutant
    }

    pub fn insert_vertex(&mut self, geometry: G::Vertex) -> VertexKey {
        self.mutant_mut().vertices.insert(Vertex::new(geometry))
    }

    pub fn connect_outgoing_edge(&mut self, a: VertexKey, ab: EdgeKey) -> Result<(), Error> {
        if a == ab.to_vertex_keys().0 {
            let source = self.mutant_mut()
                .vertex_mut(a)
                .ok_or_else(|| Error::from(GraphError::TopologyNotFound))?;
            source.edge = Some(ab);
            Ok(())
        }
        else {
            Err(Error::from(GraphError::TopologyMalformed))
        }
    }

    pub fn disconnect_outgoing_edge(&mut self, a: VertexKey) -> Result<EdgeKey, Error> {
        let source = self.mutant_mut()
            .vertex_mut(a)
            .ok_or_else(|| Error::from(GraphError::TopologyNotFound))?;
        if let Some(ax) = source.edge.take() {
            Ok(ax)
        }
        else {
            Err(Error::from(GraphError::TopologyConflict))
        }
    }
}

impl<G> Commit<G> for VertexMutation<G>
where
    G: Geometry,
{
    type Error = Error;

    fn commit(self) -> Result<Self::Mutant, Self::Error> {
        let VertexMutation { mutant, touched } = self;
        for vertex in touched {}
        Ok(mutant)
    }
}

impl<G> Mode<G> for VertexMutation<G>
where
    G: Geometry,
{
    type Mutant = Mesh<G, Inconsistent>;
}

impl<G> Mutate<G> for VertexMutation<G>
where
    G: Geometry,
{
    fn mutate(mutant: Self::Mutant) -> Self {
        VertexMutation {
            mutant: mutant,
            touched: Default::default(),
        }
    }
}
