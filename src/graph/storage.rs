//! Storage for topological data in a mesh.

use std::collections::HashMap;
use std::hash::Hash;

use self::alias::*;
use geometry::Attribute;

pub trait Topological {
    type Key: OpaqueKey;
    type Attribute: Attribute;
}

pub trait ImplicitKey: Copy + Default + Sized {
    fn into_next_key(self) -> Self;
}

impl ImplicitKey for () {
    fn into_next_key(self) -> Self {}
}

impl ImplicitKey for u64 {
    fn into_next_key(self) -> Self {
        self + 1
    }
}

pub trait OpaqueKey: Copy + Sized {
    type Inner: Copy + Eq + Hash + Into<Self>;
    type Implicit: ImplicitKey;

    fn into_inner(self) -> Self::Inner;
}

#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub struct VertexKey(u64);

impl From<u64> for VertexKey {
    fn from(key: u64) -> Self {
        VertexKey(key)
    }
}

impl OpaqueKey for VertexKey {
    type Inner = u64;
    type Implicit = u64;

    fn into_inner(self) -> Self::Inner {
        self.0
    }
}

#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub struct EdgeKey(u64, u64);

impl EdgeKey {
    // TODO: This may be useful in some existing code that constructs the
    //       opposite edge key.
    #[allow(dead_code)]
    pub(in graph) fn to_opposite_key(&self) -> EdgeKey {
        EdgeKey(self.1, self.0)
    }

    pub(in graph) fn to_vertex_keys(&self) -> (VertexKey, VertexKey) {
        (self.0.into(), self.1.into())
    }
}

impl OpaqueKey for EdgeKey {
    type Inner = (u64, u64);
    type Implicit = ();

    fn into_inner(self) -> Self::Inner {
        (self.0, self.1)
    }
}

impl From<(u64, u64)> for EdgeKey {
    fn from(key: (u64, u64)) -> Self {
        EdgeKey(key.0, key.1)
    }
}

impl From<(VertexKey, VertexKey)> for EdgeKey {
    fn from(key: (VertexKey, VertexKey)) -> Self {
        EdgeKey(key.0.into_inner(), key.1.into_inner())
    }
}

#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub struct FaceKey(u64);

impl From<u64> for FaceKey {
    fn from(key: u64) -> Self {
        FaceKey(key)
    }
}

impl OpaqueKey for FaceKey {
    type Inner = u64;
    type Implicit = u64;

    fn into_inner(self) -> Self::Inner {
        self.0
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Key {
    Vertex(VertexKey),
    Edge(EdgeKey),
    Face(FaceKey),
}

impl From<VertexKey> for Key {
    fn from(key: VertexKey) -> Self {
        Key::Vertex(key)
    }
}

impl From<EdgeKey> for Key {
    fn from(key: EdgeKey) -> Self {
        Key::Edge(key)
    }
}

impl From<FaceKey> for Key {
    fn from(key: FaceKey) -> Self {
        Key::Face(key)
    }
}

pub trait AsStorage<T>
where
    T: Topological,
{
    fn as_storage(&self) -> &Storage<T>;
}

impl<'a, T, U> AsStorage<T> for &'a U
where
    T: Topological,
    U: AsStorage<T>,
{
    fn as_storage(&self) -> &Storage<T> {
        <U as AsStorage<T>>::as_storage(self)
    }
}

pub trait AsStorageMut<T>
where
    T: Topological,
{
    fn as_storage_mut(&mut self) -> &mut Storage<T>;
}

impl<'a, T, U> AsStorageMut<T> for &'a mut U
where
    T: Topological,
    U: AsStorageMut<T>,
{
    fn as_storage_mut(&mut self) -> &mut Storage<T> {
        <U as AsStorageMut<T>>::as_storage_mut(self)
    }
}

#[derive(Clone, Default)]
pub struct Storage<T>
where
    T: Topological,
{
    implicit: InnerImplicitKey<T>,
    hash: HashMap<InnerKey<T>, T>,
}

impl<T> Storage<T>
where
    T: Topological,
{
    pub fn new() -> Self {
        Storage {
            implicit: Default::default(),
            hash: HashMap::new(),
        }
    }

    // This function isn't strictly necessary, because `HashMap::new` begins
    // with a capacity of zero and does not allocate (and is used in
    // `Storage::new`). However, `Storage` abstracts its underlying data
    // structure, so the notion of an unallocated and empty container is
    // explicit.
    pub fn empty() -> Self {
        Self::new()
    }

    pub fn map_values_into<U, F>(self, mut f: F) -> Storage<U>
    where
        U: Topological<Key = T::Key>,
        F: FnMut(T) -> U,
    {
        let mut hash = HashMap::new();
        for (key, value) in self.hash {
            hash.insert(key, f(value));
        }
        Storage {
            implicit: self.implicit,
            hash: hash,
        }
    }

    pub fn len(&self) -> usize {
        self.hash.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = (&InnerKey<T>, &T)> {
        self.hash.iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (&InnerKey<T>, &mut T)> {
        self.hash.iter_mut()
    }

    pub fn keys(&self) -> impl Iterator<Item = &InnerKey<T>> {
        self.hash.keys()
    }

    pub fn contains_key(&self, key: &T::Key) -> bool {
        self.hash.contains_key(&key.into_inner())
    }

    pub fn get(&self, key: &T::Key) -> Option<&T> {
        self.hash.get(&key.into_inner())
    }

    pub fn get_mut(&mut self, key: &T::Key) -> Option<&mut T> {
        self.hash.get_mut(&key.into_inner())
    }

    pub fn insert_with_key(&mut self, key: &T::Key, item: T) -> Option<T> {
        self.hash.insert(key.into_inner(), item)
    }

    pub fn remove(&mut self, key: &T::Key) -> Option<T> {
        self.hash.remove(&key.into_inner())
    }
}

impl<T, K> Storage<T>
where
    T: Topological,
    T::Key: From<K> + OpaqueKey<Inner = K, Implicit = K>,
    K: Eq + Hash + ImplicitKey,
{
    pub fn insert(&mut self, item: T) -> T::Key {
        let key = self.implicit;
        self.hash.insert(key, item);
        self.implicit = self.implicit.into_next_key();
        key.into()
    }
}

impl<T> AsStorage<T> for Storage<T>
where
    T: Topological,
{
    fn as_storage(&self) -> &Storage<T> {
        self
    }
}

impl<T> AsStorageMut<T> for Storage<T>
where
    T: Topological,
{
    fn as_storage_mut(&mut self) -> &mut Storage<T> {
        self
    }
}

pub trait Bind<T, M>
where
    T: Topological,
    M: AsStorage<T>,
{
    type Output;

    fn bind(self, source: M) -> Self::Output;
}

pub mod alias {
    use super::*;

    pub type InnerKey<T> = <<T as Topological>::Key as OpaqueKey>::Inner;
    pub type InnerImplicitKey<T> = <<T as Topological>::Key as OpaqueKey>::Implicit;
}
