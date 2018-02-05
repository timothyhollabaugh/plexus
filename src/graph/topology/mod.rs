// This code assumes that any keys for topological structures in the mesh are
// valid (hence the `unwrap` calls), which is very important for `Deref`.
// Topological mutations using views are dangerous if they do not consume
// `self`. If these views can be used to mutate that data, then they can also
// invalidate these constraints and cause panics. Any mutating functions should
// consume the view.
//
// Similarly, toplogical mutations could invalidate views used to reach other
// views. This means that it is unsafe for a mutable view to yield another
// mutable view, because the second view may cause mutations that invalidate
// the first. Circulators effectively map from a mutable view to orphan views,
// for example. While `into` and immutable accessor functions are okay, mutable
// accessor functions MUST yield orphans (or not exist at all).

use geometry::{Attribute, Geometry};
use graph::mesh::{Consistency, Consistent, Mesh};
use graph::storage::OpaqueKey;

pub mod edge;
pub mod face;
pub mod vertex;

pub use self::edge::{EdgeKeyTopology, EdgeView, OrphanEdgeView};
pub use self::face::{FaceKeyTopology, FaceView, OrphanFaceView};
pub use self::vertex::{OrphanVertexView, VertexView};

pub type EdgeRef<'a, G, C = Consistent> = EdgeView<&'a Mesh<G, C>, G, C>;
pub type EdgeMut<'a, G, C = Consistent> = EdgeView<&'a mut Mesh<G, C>, G, C>;
pub type OrphanEdgeMut<'a, G> = OrphanEdgeView<'a, G>;

pub type FaceRef<'a, G, C = Consistent> = FaceView<&'a Mesh<G, C>, G, C>;
pub type FaceMut<'a, G, C = Consistent> = FaceView<&'a mut Mesh<G, C>, G, C>;
pub type OrphanFaceMut<'a, G> = OrphanFaceView<'a, G>;

pub type VertexRef<'a, G, C = Consistent> = VertexView<&'a Mesh<G, C>, G, C>;
pub type VertexMut<'a, G, C = Consistent> = VertexView<&'a mut Mesh<G, C>, G, C>;
pub type OrphanVertexMut<'a, G> = OrphanVertexView<'a, G>;

pub trait Topological {
    type Key: OpaqueKey;
    type Attribute: Attribute;
}

// TODO: The view traits could require `Deref` and replace the `Topology`
//       associated type with `Target`. However, they are only used to abstract
//       over the instantiation of views, and placing constraints on
//       `Deref::Target` is not transient.

pub trait View<M, G, C>
where
    M: AsRef<Mesh<G, C>>,
    G: Geometry,
    C: Consistency,
{
    type Topology: Topological;

    fn from_mesh(mesh: M, key: <Self::Topology as Topological>::Key) -> Self;
}

// Orphan views do not abstract over mutability of the topology they reference,
// as an immutable orphan view is not useful and could always be replaced by an
// immutable view.
pub trait OrphanView<'a, G>
where
    G: Geometry,
{
    type Topology: Topological;

    fn from_topology(
        topology: &'a mut Self::Topology,
        key: <Self::Topology as Topological>::Key,
    ) -> Self;
}
