use std::collections::HashMap;
use std::hash::Hash;
use std::cmp::Eq;

#[derive(Default, Debug)]
pub struct StateStack<'a, T: 'a, V: 'a>  where T: Hash + Eq {
  parent: Option<&'a StateStack<'a, T, V>>,
  values: HashMap<T, V>
}

impl<'a, T, V> StateStack<'a, T, V> where T: Hash + Eq {
  pub fn insert(&mut self, key: T, value: V) where T: Hash + Eq {
    self.values.insert(key, value);
  }
  pub fn lookup(&self, key: &T) -> Option<&V> where T: Hash + Eq {
    match (self.values.get(key), self.parent) {
      (None, None) => None,
      (None, Some(parent)) => parent.lookup(key),
      (Some(value), _) => Some(value),
    }
  }
}

