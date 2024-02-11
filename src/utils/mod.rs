use std::hash::{DefaultHasher, Hash, Hasher};

pub mod btreemap2;

pub fn hash<T: Hash>(t: &T) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    s.finish()
}
