#![cfg(test)]

use nalgebra::{Point2, Point3, Scalar};
use num::NumCast;

use geometry::{Attribute, Geometry};
use geometry::convert::{AsPosition, FromGeometry, IntoGeometry};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Input {
    Source,
    Destination,
}

pub use self::Input::Source;
pub use self::Input::Destination;

pub type Operand = Option<Input>;

pub trait OperandAnnotated {
    fn is_source(&self) -> bool;
    fn is_destination(&self) -> bool;
}

impl<T> OperandAnnotated for (T, Operand) {
    fn is_source(&self) -> bool {
        match self.1 {
            Some(Source) => true,
            _ => false,
        }
    }

    fn is_destination(&self) -> bool {
        match self.1 {
            Some(Destination) => true,
            _ => false,
        }
    }
}

pub const SOURCE: Operand = Some(Source);
pub const DESTINATION: Operand = Some(Destination);

impl<A, T> Attribute for (T, A)
where
    A: Clone + Default,
    T: Attribute,
{
}

impl<A, T> FromGeometry<((T, T), A)> for (Point2<T>, A)
where
    A: Clone + Default,
    T: NumCast + Scalar,
{
    fn from_geometry(other: ((T, T), A)) -> Self {
        (other.0.into_geometry(), other.1)
    }
}

impl<A, T> FromGeometry<((T, T, T), A)> for (Point3<T>, A)
where
    A: Clone + Default,
    T: NumCast + Scalar,
{
    fn from_geometry(other: ((T, T, T), A)) -> Self {
        (other.0.into_geometry(), other.1)
    }
}

pub struct Annotated<A, V = (), E = (), F = ()>
where
    A: Clone + Default,
    V: Attribute,
    E: Attribute + Default,
    F: Attribute + Default,
{
    pub vertex: (V, A),
    pub edge: (E, A),
    pub face: (F, A),
}

impl<A, V, E, F> Geometry for Annotated<A, V, E, F>
where
    A: Clone + Default,
    V: Attribute,
    E: Attribute + Default,
    F: Attribute + Default,
{
    type Vertex = (V, A);
    type Edge = ();
    type Face = (F, A);
}

impl<A, V, E, F> AsPosition for Annotated<A, V, E, F>
where
    A: Clone + Default,
    V: AsPosition + Attribute,
    E: Attribute + Default,
    F: Attribute + Default,
{
    type Target = <V as AsPosition>::Target;

    fn as_position(&self) -> &Self::Target {
        self.vertex.0.as_position()
    }

    fn as_position_mut(&mut self) -> &mut Self::Target {
        self.vertex.0.as_position_mut()
    }
}
