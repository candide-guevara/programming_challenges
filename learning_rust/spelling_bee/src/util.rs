use std::collections::BTreeSet;
use std::slice;

pub fn alphabet() -> Vec<char> {
  return ('a'..='z').collect::<Vec<char>>();
}

pub fn as_eternal(s: &str) -> &'static str {
  return unsafe {
    std::str::from_utf8_unchecked(
      slice::from_raw_parts(s.as_ptr(), s.len()))
  };
}

pub fn as_some_eternal(s: &String) -> Option<&'static str> {
  return Some(as_eternal(s.as_str()));
}

pub fn as_some_eternal_slice<T>(v: &Vec::<T>) -> Option<&'static [T]> {
  return unsafe {
    Some(slice::from_raw_parts(v.as_ptr(), v.len()))
  };
}

pub fn to_unique_letters(s: &String) -> String {
  return String::from_iter(
    BTreeSet::from_iter(s.chars()).iter()
  );
}

pub trait Head {
  type ValT;
  fn head(&self, t: usize) -> Vec::<Self::ValT>;
}

impl<T> Head for Vec<T> where T: Clone {
  type ValT = T;
  fn head(&self, t: usize) -> Vec::<Self::ValT> {
    return self.iter().take(t).map(|x| x.clone()).collect::<Vec::<Self::ValT>>();
  }
}


