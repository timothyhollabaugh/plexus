#![cfg(feature = "serialize-ply")]

use decorum::{Ordered, Primitive};
use num::Float;
use ply_rs::ply::{self, DefaultElement, PropertyAccess};
use std::io::{Read, Write};

use geometry::{Attribute, Geometry, Triplet};
use graph::Mesh;

pub use ply_rs::ply::{Encoding, PropertyType, ScalarType};

pub type PropertyMap = DefaultElement;

pub trait Property: Sized {
    const PLY_TYPE: PropertyType;

    fn read<A, N>(name: N, properties: &A) -> Result<Self, ()>
    where
        A: PropertyAccess,
        N: Into<String>;

    fn write<A, N>(&self, name: N, properties: &mut A)
    where
        A: PropertyAccess,
        N: Into<String>;
}

impl Property for f32 {
    const PLY_TYPE: PropertyType = PropertyType::Scalar(ScalarType::Float);

    fn read<A, N>(name: N, properties: &A) -> Result<Self, ()>
    where
        A: PropertyAccess,
        N: Into<String>,
    {
        properties.get_float(&name.into()).ok_or(())
    }

    fn write<A, N>(&self, name: N, properties: &mut A)
    where
        A: PropertyAccess,
        N: Into<String>,
    {
        properties.set_property(name.into(), ply::Property::Float(*self));
    }
}

impl Property for f64 {
    const PLY_TYPE: PropertyType = PropertyType::Scalar(ScalarType::Double);

    fn read<A, N>(name: N, properties: &A) -> Result<Self, ()>
    where
        A: PropertyAccess,
        N: Into<String>,
    {
        properties.get_double(&name.into()).ok_or(())
    }

    fn write<A, N>(&self, name: N, properties: &mut A)
    where
        A: PropertyAccess,
        N: Into<String>,
    {
        properties.set_property(name.into(), ply::Property::Double(*self));
    }
}

impl<T> Property for Ordered<T>
where
    T: Float + Primitive + Property,
{
    const PLY_TYPE: PropertyType = T::PLY_TYPE;

    fn read<A, N>(name: N, properties: &A) -> Result<Self, ()>
    where
        A: PropertyAccess,
        N: Into<String>,
    {
        Ok(Ordered::from_raw_float(<T as Property>::read(
            name,
            properties,
        )?))
    }

    fn write<A, N>(&self, name: N, properties: &mut A)
    where
        A: PropertyAccess,
        N: Into<String>,
    {
        self.clone().into_raw_float().write(name, properties);
    }
}

pub trait Element: Attribute {
    fn property_definitions<'a>() -> &'a [(&'static str, PropertyType)];

    fn read<A>(properties: &A) -> Result<Self, ()>
    where
        A: PropertyAccess;

    fn write<A>(&self, properties: &mut A) -> Result<(), ()>
    where
        A: PropertyAccess;
}

impl Element for () {
    fn property_definitions<'a>() -> &'a [(&'static str, PropertyType)] {
        &[]
    }

    fn read<A>(_: &A) -> Result<Self, ()>
    where
        A: PropertyAccess,
    {
        Ok(())
    }

    fn write<A>(&self, _: &mut A) -> Result<(), ()>
    where
        A: PropertyAccess,
    {
        Ok(())
    }
}

impl<P> Element for Triplet<P>
where
    P: Clone + Property,
{
    fn property_definitions<'a>() -> &'a [(&'static str, PropertyType)] {
        &[("x", P::PLY_TYPE), ("y", P::PLY_TYPE), ("z", P::PLY_TYPE)]
    }

    fn read<A>(properties: &A) -> Result<Self, ()>
    where
        A: PropertyAccess,
    {
        let mut values = Self::property_definitions()
            .iter()
            .map(|&(name, _)| P::read(name, properties).unwrap());
        Ok(Triplet(
            values.next().unwrap(),
            values.next().unwrap(),
            values.next().unwrap(),
        ))
    }

    fn write<A>(&self, properties: &mut A) -> Result<(), ()>
    where
        A: PropertyAccess,
    {
        self.0.write("x", properties);
        self.1.write("y", properties);
        self.2.write("z", properties);
        Ok(())
    }
}

pub trait Ply: Geometry {
    fn read<T>(input: &T) -> Result<Mesh<Self>, ()>
    where
        T: Read;

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
    fn read<T>(input: &T) -> Result<Mesh<Self>, ()>
    where
        T: Read,
    {
        Err(())
    }

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
        fn property_definitions<'a>() -> &'a [(&'static str, PropertyType)] {
            &[("x", P::PLY_TYPE), ("y", P::PLY_TYPE), ("z", P::PLY_TYPE)]
        }

        fn read<A>(properties: &A) -> Result<Self, ()>
        where
            A: PropertyAccess,
        {
            let mut values = Self::property_definitions()
                .iter()
                .map(|&(name, _)| P::read(name, properties).unwrap());
            Ok(Point3::new(
                values.next().unwrap(),
                values.next().unwrap(),
                values.next().unwrap(),
            ))
        }

        fn write<A>(&self, properties: &mut A) -> Result<(), ()>
        where
            A: PropertyAccess,
        {
            self.x.write("x", properties);
            self.y.write("y", properties);
            self.z.write("z", properties);
            Ok(())
        }
    }
}
