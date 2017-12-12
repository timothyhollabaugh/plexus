#![cfg(feature = "serialize-ply")]

use decorum::{Ordered, Primitive};
use num::Float;
use std::io::{Read, Write};

use geometry::{Attribute, Geometry, Triplet};
use graph::Mesh;

pub use ply_rs::ply::{Encoding, PropertyType, ScalarType};

pub trait Property {
    const PLY_TYPE: PropertyType;
}

impl Property for f32 {
    const PLY_TYPE: PropertyType = PropertyType::Scalar(ScalarType::Float);
}

impl Property for f64 {
    const PLY_TYPE: PropertyType = PropertyType::Scalar(ScalarType::Double);
}

impl<T> Property for Ordered<T>
where
    T: Float + Primitive + Property,
{
    const PLY_TYPE: PropertyType = T::PLY_TYPE;
}

pub trait Element: Attribute {
    fn properties<'a>() -> &'a [(&'static str, PropertyType)];

    fn read<T>() -> Result<Self, ()>
    where
        T: Read;

    fn write<T>(&self) -> Result<(), ()>
    where
        T: Write;
}

impl Element for () {
    fn properties<'a>() -> &'a [(&'static str, PropertyType)] {
        &[]
    }

    fn read<T>() -> Result<Self, ()>
    where
        T: Read,
    {
        Ok(())
    }

    fn write<T>(&self) -> Result<(), ()>
    where
        T: Write,
    {
        Ok(())
    }
}

impl<P> Element for Triplet<P>
where
    P: Clone + Property,
{
    fn properties<'a>() -> &'a [(&'static str, PropertyType)] {
        &[("x", P::PLY_TYPE), ("y", P::PLY_TYPE), ("z", P::PLY_TYPE)]
    }

    fn read<T>() -> Result<Self, ()>
    where
        T: Read,
    {
        Err(())
    }

    fn write<T>(&self) -> Result<(), ()>
    where
        T: Write,
    {
        Err(())
    }
}

pub trait Ply: Geometry {
    fn write<T>(output: &mut T, encoding: Encoding, mesh: &Mesh<Self>) -> Result<(), ()>
    where
        T: Write;
}

impl<G> Ply for G
where
    G: Geometry,
    G::Vertex: Element,
    G::Edge: Element,
    G::Face: Element,
{
    fn write<T>(output: &mut T, encoding: Encoding, mesh: &Mesh<Self>) -> Result<(), ()>
    where
        T: Write,
    {
        Err(())
    }
}

#[cfg(feature = "geometry-nalgebra")]
mod feature_geometry_nalgebra {
    use nalgebra::{Point3, Scalar};

    use super::*;

    impl<P> Element for Point3<P>
    where
        P: Property + Scalar,
    {
        fn properties<'a>() -> &'a [(&'static str, PropertyType)] {
            &[("x", P::PLY_TYPE), ("y", P::PLY_TYPE), ("z", P::PLY_TYPE)]
        }

        fn read<T>() -> Result<Self, ()>
        where
            T: Read,
        {
            Err(())
        }

        fn write<T>(&self) -> Result<(), ()>
        where
            T: Write,
        {
            Err(())
        }
    }
}
