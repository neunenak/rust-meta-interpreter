use std::collections::HashMap;
use std::hash::Hash;
use std::cmp::Eq;

#[derive(Default, Debug)]
pub struct ScopeStack<'a, T: 'a, V: 'a>  where T: Hash + Eq {
  parent: Option<&'a ScopeStack<'a, T, V>>,
  values: HashMap<T, V>,
  scope_name: Option<String>
}

impl<'a, T, V> ScopeStack<'a, T, V> where T: Hash + Eq {
  pub fn new(name: Option<String>) -> ScopeStack<'a, T, V> where T: Hash + Eq {
    ScopeStack {
      parent: None,
      values: HashMap::new(),
      scope_name: name
    }
  }
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

  pub fn new_scope(&'a self, name: Option<String>) -> ScopeStack<'a, T, V> where T: Hash + Eq {
    ScopeStack {
      parent: Some(self),
      values: HashMap::default(),
      scope_name: name,
    }
  }
  pub fn get_name(&self) -> Option<&String> {
    self.scope_name.as_ref()
  }
}

