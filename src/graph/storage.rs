/// Storage for topological data in a mesh.

#[cfg(feature = "serialize-serde")]
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::collections::hash_map::{Iter, IterMut};
use std::hash::Hash;
use std::ops::{Deref, DerefMut};

use graph::topology::Topological;

#[cfg_attr(feature = "serialize-serde", derive(Deserialize, Serialize))]
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub struct Key(u64);

impl Key {
    // TODO: This allows tests to be more terse, but in a very fragile way.
    //       Replace this with an alternative way to find relevant topology.
    #[cfg(test)]
    pub(crate) fn new(value: u64) -> Self {
        Key(value)
    }
}

impl Deref for Key {
    type Target = u64;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Key {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

pub trait Generator: Copy + Default {
    fn next(&self) -> Self;
}

impl Generator for () {
    fn next(&self) -> Self {}
}

impl Generator for Key {
    fn next(&self) -> Self {
        Key(**self + 1)
    }
}

pub trait OpaqueKey: Sized {
    type RawKey: Copy + Eq + Hash + Into<Self>;
    type Generator: Generator;

    fn to_inner(&self) -> Self::RawKey;
}

#[cfg_attr(feature = "serialize-serde", derive(Deserialize, Serialize))]
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub struct VertexKey(Key);

impl From<Key> for VertexKey {
    fn from(key: Key) -> Self {
        VertexKey(key)
    }
}

impl OpaqueKey for VertexKey {
    type RawKey = Key;
    type Generator = Key;

    fn to_inner(&self) -> Self::RawKey {
        self.0
    }
}

#[cfg_attr(feature = "serialize-serde", derive(Deserialize, Serialize))]
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub struct EdgeKey(Key, Key);

impl EdgeKey {
    // TODO: This may be useful in some existing code that constructs the
    //       opposite edge key.
    #[allow(dead_code)]
    pub(crate) fn to_opposite_key(&self) -> EdgeKey {
        EdgeKey(self.1, self.0)
    }

    pub(crate) fn to_vertex_keys(&self) -> (VertexKey, VertexKey) {
        (self.0.into(), self.1.into())
    }
}

impl OpaqueKey for EdgeKey {
    type RawKey = (Key, Key);
    type Generator = ();

    fn to_inner(&self) -> Self::RawKey {
        (self.0, self.1)
    }
}

impl From<(Key, Key)> for EdgeKey {
    fn from(key: (Key, Key)) -> Self {
        EdgeKey(key.0, key.1)
    }
}

impl From<(VertexKey, VertexKey)> for EdgeKey {
    fn from(key: (VertexKey, VertexKey)) -> Self {
        EdgeKey(key.0.to_inner(), key.1.to_inner())
    }
}

#[cfg_attr(feature = "serialize-serde", derive(Deserialize, Serialize))]
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub struct FaceKey(Key);

impl From<Key> for FaceKey {
    fn from(key: Key) -> Self {
        FaceKey(key)
    }
}

impl OpaqueKey for FaceKey {
    type RawKey = Key;
    type Generator = Key;

    fn to_inner(&self) -> Self::RawKey {
        self.0
    }
}

pub type StorageIter<'a, T> = Iter<'a, <<T as Topological>::Key as OpaqueKey>::RawKey, T>;
pub type StorageIterMut<'a, T> = IterMut<'a, <<T as Topological>::Key as OpaqueKey>::RawKey, T>;

#[cfg_attr(feature = "serialize-serde", derive(Deserialize, Serialize))]
pub struct Storage<T>
where
    T: Topological,
{
    #[cfg_attr(feature = "serialize-serde",
               serde(bound(serialize = "<T::Key as OpaqueKey>::Generator: Serialize")))]
    #[cfg_attr(feature = "serialize-serde",
               serde(bound(deserialize = "<T::Key as OpaqueKey>::Generator: Deserialize<'de>")))]
    generator: <<T as Topological>::Key as OpaqueKey>::Generator,
    #[cfg_attr(feature = "serialize-serde",
               serde(bound(serialize = "T: Serialize, <T::Key as OpaqueKey>::RawKey: Serialize")))]
    #[cfg_attr(feature = "serialize-serde",
               serde(bound(deserialize =
                   "T: Deserialize<'de>, <T::Key as OpaqueKey>::RawKey: Deserialize<'de>")))]
    hash: HashMap<<<T as Topological>::Key as OpaqueKey>::RawKey, T>,
}

impl<T> Storage<T>
where
    T: Topological,
{
    pub fn new() -> Self {
        Storage {
            generator: Default::default(),
            hash: HashMap::new(),
        }
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
            generator: self.generator,
            hash: hash,
        }
    }

    #[inline(always)]
    pub fn insert_with_key(&mut self, key: &T::Key, item: T) -> Option<T> {
        self.hash.insert(key.to_inner(), item)
    }

    #[inline(always)]
    pub fn contains_key(&self, key: &T::Key) -> bool {
        self.hash.contains_key(&key.to_inner())
    }

    #[inline(always)]
    pub fn get(&self, key: &T::Key) -> Option<&T> {
        self.hash.get(&key.to_inner())
    }

    #[inline(always)]
    pub fn get_mut(&mut self, key: &T::Key) -> Option<&mut T> {
        self.hash.get_mut(&key.to_inner())
    }

    #[inline(always)]
    pub fn remove(&mut self, key: &T::Key) -> Option<T> {
        self.hash.remove(&key.to_inner())
    }
}

impl<T> Storage<T>
where
    T: Topological,
    T::Key: From<Key> + OpaqueKey<RawKey = Key, Generator = Key>,
{
    pub fn insert_with_generator(&mut self, item: T) -> T::Key {
        let key = self.generator;
        self.hash.insert(key, item);
        self.generator = self.generator.next();
        key.into()
    }
}

impl<T> Deref for Storage<T>
where
    T: Topological,
{
    type Target = HashMap<<<T as Topological>::Key as OpaqueKey>::RawKey, T>;

    fn deref(&self) -> &Self::Target {
        &self.hash
    }
}

impl<T> DerefMut for Storage<T>
where
    T: Topological,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.hash
    }
}
