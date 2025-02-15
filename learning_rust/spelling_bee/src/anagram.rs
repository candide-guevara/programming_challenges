mod test;
use super::config;
use super::error::*;
use super::util::*;

use regex::Regex;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::hash_map::Entry::*;
use std::fs::File;
use std::io::prelude::*;

pub struct Anagram<'a> {
  pub letters_words: HashMap::<&'a str, Vec::<&'a str>>,  
  pub letters_histo: HashMap::<char, u32>,  
  pub words: Vec::<String>,
  keys: Vec::<String>,
}

impl<'a> Anagram<'a> {
  fn push_word(&mut self, k: String, i: usize) {
    let k2 = as_eternal(k.as_str());
    let v = as_eternal(self.words.get(i).unwrap());
    match self.letters_words.entry(k2) {
      Occupied(mut e) => e.get_mut().push(v),
      Vacant(mut e)   => {
        let mut vs = e.insert(Vec::<&str>::new());
        vs.push(v);
        self.keys.push(k);
      }
    };
  }

  pub fn letters_by_freq(&self) -> Vec<char> {
    let mut tuples = self.letters_histo.iter().map(|(k,v)| (*v,*k))
                                       .collect::<Vec<(u32,char)>>();
    tuples.sort();
    return tuples.into_iter()
                 .rev().map(|(v,k)| k).collect();
  }
}

pub fn from_dict(conf: &config::Config) -> StatusOr::<Anagram> {
  let mut words = read_dict(conf)?;
  let cap = words.len();
  let keys: Vec::<String> = words.iter()
                                 .map(to_unique_letters).collect();
  let mut anagram = Anagram{
    letters_words: HashMap::with_capacity(cap),
    letters_histo: HashMap::new(),
    words: words,
    keys: Vec::with_capacity(cap),
  };

  for (i, k) in keys.into_iter().enumerate() {
    for c in k.chars() {
      *anagram.letters_histo.entry(c).or_insert(0) += 1;
    }
    anagram.push_word(k, i);
  }
  return Ok(anagram);
}

fn remove_plurals(words: Vec::<String>) -> StatusOr::<Vec::<String>> {
  let mut last = String::new();
  let mut has  = HashSet::<&str>::with_capacity(words.len());
  let mut r = Vec::with_capacity(words.len());
  has.extend(words.iter().map(|s| s.as_str()));
  for w in words.iter() {
    last.clear();
    last.push_str(w.as_str());
    last.pop();
    if !has.contains(last.as_str()) { r.push(w.clone()); }
  }
  if words.len() == r.len() {
    return Error::new(Code::INTERNAL, "did not find plurals");
  }
  if r.len() == 0 {
    return Error::new(Code::PRECONDITION, "dict contained only plurals");
  }
  return Ok(r);
}

fn read_dict(conf: &config::Config) -> StatusOr::<Vec::<String>> {
  let mut file = File::open(&conf.input_path)?;
  let mut content = String::with_capacity(1024 * 1024 * 8);
  let filter_rx = Regex::new(&conf.word_filter_rx)?;

  file.read_to_string(&mut content)?;
  let mut words = content.rsplit('\n')
                         .filter(|s| filter_rx.is_match(*s))
                         .map(String::from)
                         .collect::<Vec<String>>();
  if words.len() == 0 {
    return Error::new(Code::PRECONDITION, "empty dictionary");
  }
  let mut no_plural = remove_plurals(words)?;
  return Ok(no_plural.into_iter()
                     .filter(|s| s.chars().count() >= conf.min_len)
                     .filter(|s| s.chars().count() <= conf.max_len)
                     .collect()
  );
}

