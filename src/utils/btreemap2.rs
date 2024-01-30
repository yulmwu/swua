use std::{borrow::Borrow, collections::BTreeMap};

#[derive(Clone, Debug)]
pub struct BTreeMap2<K1, K2, V>(BTreeMap<K1, BTreeMap<K2, V>>);

impl<K1, K2, V> BTreeMap2<K1, K2, V> {
    #[inline]
    #[must_use]
    pub fn new() -> Self {
        Self(BTreeMap::new())
    }

    #[inline]
    #[must_use]
    pub fn get<T, U>(&self, k1: &T, k2: &U) -> Option<&V>
    where
        K1: Borrow<T> + Ord,
        K2: Borrow<U> + Ord,
        T: Ord + ?Sized,
        U: Ord + ?Sized,
    {
        self.0.get(k1).and_then(|map| map.get(k2))
    }

    #[inline]
    #[must_use]
    pub fn get_mut<T, U>(&mut self, k1: &T, k2: &U) -> Option<&mut V>
    where
        K1: Borrow<T> + Ord,
        K2: Borrow<U> + Ord,
        T: Ord + ?Sized,
        U: Ord + ?Sized,
    {
        self.0.get_mut(k1).and_then(|map| map.get_mut(k2))
    }

    #[inline]
    #[must_use]
    pub fn get_key1<T>(&self, k1: &T) -> Option<&BTreeMap<K2, V>>
    where
        K1: Borrow<T> + Ord,
        T: Ord + ?Sized,
    {
        self.0.get(k1)
    }

    #[inline]
    #[must_use]
    pub fn get_key1_mut<T>(&mut self, k1: &T) -> Option<&mut BTreeMap<K2, V>>
    where
        K1: Borrow<T> + Ord,
        T: Ord + ?Sized,
    {
        self.0.get_mut(k1)
    }

    // #[inline]
    // #[must_use]
    // pub fn get_key2<T, U>(&self, k2: &U) -> Option<&BTreeMap<K1, V>>
    // where
    //     K1: Borrow<T> + Ord,
    //     K2: Borrow<U> + Ord,
    //     U: Ord + ?Sized,
    // {
    //     todo!()
    // }

    // #[inline]
    // #[must_use]
    // pub fn get_key2_mut<T, U>(&mut self, k2: &U) -> Option<&mut BTreeMap<K1, V>>
    // where
    //     K1: Borrow<T> + Ord,
    //     K2: Borrow<U> + Ord,
    //     U: Ord + ?Sized,
    // {
    //     todo!()
    // }

    #[inline]
    #[must_use]
    pub fn contains_key<T, U>(&self, k1: &T, k2: &U) -> bool
    where
        K1: Borrow<T> + Ord,
        K2: Borrow<U> + Ord,
        T: Ord + ?Sized,
        U: Ord + ?Sized,
    {
        if let Some(map) = self.0.get(k1) {
            map.contains_key(k2)
        } else {
            false
        }
    }

    #[inline]
    #[must_use]
    pub fn contains_key1<T>(&self, k1: &T) -> bool
    where
        K1: Borrow<T> + Ord,
        T: Ord + ?Sized,
    {
        self.0.contains_key(k1)
    }

    #[inline]
    #[must_use]
    pub fn contains_key2<T>(&self, k2: &T) -> bool
    where
        K2: Borrow<T> + Ord,
        T: Ord + ?Sized,
    {
        for map in self.0.values() {
            if map.contains_key(k2) {
                return true;
            }
        }
        false
    }

    #[inline]
    #[must_use]
    pub fn insert(&mut self, k1: K1, k2: K2, v: V) -> Option<V>
    where
        K1: Ord,
        K2: Ord,
    {
        self.0.entry(k1).or_default().insert(k2, v)
    }

    #[inline]
    #[must_use]
    pub fn remove<T, U>(&mut self, k1: &T, k2: &U) -> Option<V>
    where
        K1: Borrow<T> + Ord,
        K2: Borrow<U> + Ord,
        T: Ord + ?Sized,
        U: Ord + ?Sized,
    {
        if let Some(map) = self.0.get_mut(k1) {
            map.remove(k2)
        } else {
            None
        }
    }
}

impl<K1, K2, V> Default for BTreeMap2<K1, K2, V> {
    #[inline]
    #[must_use]
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_map() -> BTreeMap2<&'static str, &'static str, i32> {
        let mut map = BTreeMap2::new();
        assert_eq!(map.insert("a", "a", 5), None);
        assert_eq!(map.insert("a", "b", 2), None);
        assert_eq!(map.insert("b", "a", 3), None);
        assert_eq!(map.insert("b", "b", 4), None);
        assert_eq!(map.insert("a", "a", 1), Some(5));
        map
    }

    #[test]
    fn test_get() {
        let map = test_map();
        assert_eq!(map.get("a", "a"), Some(&1));
        assert_eq!(map.get("a", "b"), Some(&2));
        assert_eq!(map.get("b", "a"), Some(&3));
        assert_eq!(map.get("b", "b"), Some(&4));
        assert_eq!(map.get("c", "c"), None);
    }

    #[test]
    fn test_get_mut() {
        let mut map = test_map();
        assert_eq!(map.get_mut("a", "a"), Some(&mut 1));
        assert_eq!(map.get_mut("a", "b"), Some(&mut 2));
        assert_eq!(map.get_mut("b", "a"), Some(&mut 3));
        assert_eq!(map.get_mut("b", "b"), Some(&mut 4));
        assert_eq!(map.get_mut("c", "c"), None);
    }

    #[test]
    fn test_get_key1() {
        let map = test_map();
        assert_eq!(
            map.get_key1("a"),
            Some(&[("a", 1), ("b", 2)].iter().cloned().collect())
        );
        assert_eq!(
            map.get_key1("b"),
            Some(&[("a", 3), ("b", 4)].iter().cloned().collect())
        );
        assert_eq!(map.get_key1("c"), None);
    }

    #[test]
    fn test_get_key1_mut() {
        let mut map = test_map();
        assert_eq!(
            map.get_key1_mut("a"),
            Some(&mut [("a", 1), ("b", 2)].iter().cloned().collect())
        );
        assert_eq!(
            map.get_key1_mut("b"),
            Some(&mut [("a", 3), ("b", 4)].iter().cloned().collect())
        );
        assert_eq!(map.get_key1_mut("c"), None);
    }

    #[test]
    fn test_contains_key() {
        let map = test_map();
        assert!(map.contains_key("a", "a"));
        assert!(map.contains_key("a", "b"));
        assert!(map.contains_key("b", "a"));
        assert!(map.contains_key("b", "b"));
        assert!(!map.contains_key("c", "c"));
    }

    #[test]
    fn test_contains_key1() {
        let map = test_map();
        assert!(map.contains_key1("a"));
        assert!(map.contains_key1("b"));
        assert!(!map.contains_key1("c"));
    }

    #[test]
    fn test_contains_key2() {
        let map = test_map();
        assert!(map.contains_key2("a"));
        assert!(map.contains_key2("b"));
        assert!(!map.contains_key2("c"));
    }

    #[test]
    fn test_remove() {
        let mut map = test_map();
        assert_eq!(map.remove("a", "a"), Some(1));
        assert_eq!(map.remove("a", "b"), Some(2));
        assert_eq!(map.remove("b", "a"), Some(3));
        assert_eq!(map.remove("b", "b"), Some(4));
        assert_eq!(map.remove("c", "c"), None);
    }
}
