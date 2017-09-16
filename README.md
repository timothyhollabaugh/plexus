![Plexus](https://raw.githubusercontent.com/olson-sean-k/plexus/master/doc/plexus.png)

**Plexus** is a Rust library for generating and manipulating 3D meshes.

[![Build Status](https://travis-ci.org/olson-sean-k/plexus.svg?branch=master)](https://travis-ci.org/olson-sean-k/plexus)
[![Documentation](https://docs.rs/plexus/badge.svg)](https://docs.rs/plexus)
[![Crate](https://img.shields.io/crates/v/plexus.svg)](https://crates.io/crates/plexus)

## Generation and Iterator Expressions

Meshes can be generated from primitives like cubes and spheres using iterator
expressions. Primitives emit topological structures like `Triangle`s or
`Quad`s, which contain arbitrary geometric data in their vertices. These can be
transformed and decomposed into other topologies and geometric data via
triangulation, tesselation, and conversion into rendering pipeline data.

```rust
use nalgebra::Point3;
use plexus::buffer::conjoint::ConjointBuffer;
use plexus::generate::sphere;
use plexus::prelude::*; // Common traits.

// Example module in the local crate that provides rendering.
use render::{self, Vertex};

// Construct a buffer of index and vertex data from a sphere primitive. The
// vertex geometry is convertible to `Vertex` via the `From` trait in this
// example.
let buffer = sphere::UVSphere::<f32>::with_unit_radius(16, 16)
    .polygons_with_position()
    .map_vertices(|vertex| -> Point3<_> { vertex.into() })
    .map_vertices(|vertex| vertex * 10.0)
    .map_vertices(|vertex| vertex.into_hash())
    .collect::<ConjointBuffer<u32, Vertex>>();
render::draw(buffer.as_index_slice(), buffer.as_vertex_slice());
```

## Half-Edge Graph Meshes

Generators are flexible and easy to use, but only represent vertex geometry and
are difficult to query and manipulate once complete. A `Mesh`, represented as a
[half-edge graph](https://en.wikipedia.org/wiki/doubly_connected_edge_list),
supports arbitrary geometry for vertices, edges, and faces. The graph can also
be queried and manipulated in ways that generators and iterator expressions
cannot.

```rust
use nalgebra::Point3;
use plexus::generate::{sphere, LruIndexer};
use plexus::graph::{FaceKey, Mesh};
use plexus::prelude::*;

// Construct a mesh from a sphere primitive. The vertex geometry is convertible
// to `Point3<f32>` via the `FromGeometry` trait in this example.
let mesh: = sphere::UVSphere::<f32>::with_unit_radius(8, 8)
    .polygons_with_position()
    .collect_with_indexer::<Mesh<Point3<_>>, _>(LruIndexer::default());
// Extrude a face in the mesh.
let face = mesh.face_mut(FaceKey::default()).unwrap();
let face = face.extrude(1.0).unwrap();
```
