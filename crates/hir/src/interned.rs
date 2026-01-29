use std::collections::HashMap;
use std::hash::Hash;
use std::ops::Index;

use super::arena::{EntityRef, PrimaryMap};

pub struct Interned<K, V>
where
    K: EntityRef,
{
    primary: PrimaryMap<K, V>,
    index: HashMap<V, K>,
}

impl<K: EntityRef, V> Interned<K, V> {
    pub fn new() -> Self {
        Interned {
            primary: PrimaryMap::new(),
            index: HashMap::new(),
        }
    }
}

impl<K, V> Interned<K, V>
where
    K: EntityRef,
    V: Clone + Eq + Hash,
{
    pub fn intern(&mut self, v: V) -> K {
        *self
            .index
            .entry(v)
            .or_insert_with_key(|v| self.primary.push(v.clone()))
    }
}

impl<K: EntityRef, V> Default for Interned<K, V> {
    fn default() -> Self {
        Interned::new()
    }
}

impl<K: EntityRef, V> Index<K> for Interned<K, V> {
    type Output = V;

    fn index(&self, index: K) -> &V {
        &self.primary[index]
    }
}
