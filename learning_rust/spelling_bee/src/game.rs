mod test;
use super::config;
use super::anagram;
use super::util::*;
use super::error::*;

use rand;
use rand::seq::IndexedRandom;
use rand::seq::SliceRandom;
use std::collections::BTreeSet;
use std::collections::BTreeMap;
use std::collections::btree_map::Entry::*;
use std::fmt;
use std::iter;
use std::marker::PhantomData;

const kGameLen: usize = 7;
const kMinLen: usize = 4;

pub struct Game<'a> {
  main: char,
  key: String,
  solutions: BTreeMap::<usize, Vec<&'a str>>,
}
struct Generator {
  main: char,
  q: Vec<char>,
}
struct GeneratorIt<'a> {
  g: &'a Generator,
  p: PermutationIt<'a>,
  s: BTreeSet<char>,
  i: usize,
}
struct PermutationIt<'a> {
  main: char,
  p: PickIt<'a>,
  a: Vec<char>,
  w: String,
  i: usize,
}
struct PickIt<'a> {
  v: Vec<usize>,
  l: usize,
  f: bool,
  phantom: PhantomData<&'a bool>,
}

impl Generator {
  fn new(main: char) -> Generator {
    let rng = &mut rand::rng();
    let abc = alphabet();
    let mut q = abc.into_iter().filter(|&c| c != main)
                               .collect::<Vec<char>>();
    q.shuffle(rng);
    return Generator {
      main: main,
      q: q,
    };
  }

  fn take_from_q_sorted(&self, s: &mut BTreeSet::<char>, i: usize) -> Vec<char> {
    s.clear();
    s.insert(self.main);
    for j in 0..kGameLen-1 { s.insert(self.q[i + j]); }
    return Vec::<char>::from_iter(s.iter().cloned());
  }

  fn iter(&self) -> GeneratorIt {
    return GeneratorIt::new(self);
  }
}

impl<'a> GeneratorIt<'a> {
  fn new(g: &Generator) -> GeneratorIt {
    let mut s = BTreeSet::<char>::new();
    let v = g.take_from_q_sorted(&mut s, 0);
    return GeneratorIt {
      g: g,
      p: PermutationIt::new(g.main, v),
      s: s,
      i: 0,
    };
  }
  fn is_exhausted(&self) -> bool {
    return (self.g.q.len()-self.i) < (kGameLen-1);
  }
  fn advance(&mut self) -> bool {
    self.i += 1;
    if self.is_exhausted() { return false; }
    let v = self.g.take_from_q_sorted(&mut self.s, self.i);
    self.p = PermutationIt::new(self.g.main, v);
    return true;
  }
}
impl<'a> iter::Iterator for GeneratorIt<'a> {
  type Item = &'a str;
  fn next(&mut self) -> Option<Self::Item> {
    loop {
      if let Some(s) = self.p.next() { return Some(s); }
      self.i += 1;
      if self.is_exhausted() { return None; }
      let v = self.g.take_from_q_sorted(&mut self.s, self.i);
      self.p = PermutationIt::new(self.g.main,v);
    } // loop
  }
}

impl<'a> PermutationIt<'a> {
  fn new(main: char, a: Vec<char>) -> PermutationIt<'a> {
    return PermutationIt {
      main: main,
      p: PickIt::new(kGameLen),
      a: a,
      w: String::with_capacity(kGameLen*2),
      i: kGameLen,
    };
  }
}
impl<'a> iter::Iterator for PermutationIt<'a> {
  type Item = &'a str;
  fn next(&mut self) -> Option<Self::Item> {
    loop {
      if self.i < kMinLen { return None; }
      self.w.clear();
      if let Some(r) = self.p.next() {
        let mut has_main = false;
        for j in r {
          has_main |= self.main == self.a[*j];
          self.w.push(self.a[*j]);
        }
        if !has_main { continue; }
        return as_some_eternal(&self.w);
      }
      self.i -= 1;
      self.p = PickIt::new(self.i);
    } // loop
  }
}

impl<'a> PickIt<'a> {
  fn new(l: usize) -> PickIt<'a> {
    let mut it = PickIt{
      v:Vec::with_capacity(kGameLen),
      l:l,
      f:true,
      phantom: PhantomData,
    };
    for i in 0..l { it.v.push(i); }
    return it;
  }
}
impl<'a> iter::Iterator for PickIt<'a> {
  type Item = &'a [usize];
  fn next(&mut self) -> Option<Self::Item> {
    if self.f {
      self.f = false;
      return as_some_eternal_slice(&self.v);
    }
    for i in (1..=self.l) {
      let j = self.v[self.l - i];
      if j < kGameLen-i {
        for (m,n) in ((self.l - i)..self.l).enumerate() { self.v[n] = j + m + 1; }
        return as_some_eternal_slice(&self.v);
      }
    }
    return None;
  }
}

impl<'a> Game<'a> {
  fn new(main: char) -> Game<'a> {
    let mut game = Game {
      main: main,
      key: String::with_capacity(kGameLen*2),
      solutions: BTreeMap::new(),
    };
    for k in kMinLen..=kGameLen { game.solutions.insert(k, Vec::new()); }
    return game;
  }
  fn get_sol(&mut self, k: usize) -> &mut Vec<&'a str> {
    return self.solutions.get_mut(&k).unwrap();
  }
  fn valid_generation(&mut self) -> bool {
    for k in kMinLen..=kGameLen { self.get_sol(k).sort(); }
    return self.solutions.iter()
                         .all(|(k,v)| v.len() > 0);
  }
  fn reset(&mut self, key: &str, v: &Vec<&'a str>) {
    assert!(v.len() > 0);
    self.key.clear();
    self.key.push_str(key);
    for k in kMinLen..=kGameLen { self.get_sol(k).clear(); }
    self.get_sol(kGameLen).extend(v.iter());
  }
  fn add_sub_key(&mut self, key: &str, v: &Vec<&'a str>) {
    assert!(v.len() > 0);
    self.get_sol(key.len()).extend(v.iter());
  }
}
impl<'a> fmt::Display for Game<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    writeln!(f, "main = {}", self.main);
    writeln!(f, "key = {}", self.key);
    for (k,v) in self.solutions.iter().rev() {
      writeln!(f, "  {} = {:?}", k, v);
    }
    return Ok(());
  }
}

pub fn solution_for<'a>(main: char, letters: &str, words: &'a anagram::Anagram)
    -> StatusOr<Game<'a>> {
  let mut sorted_letters = letters.chars()
                                  .collect::<Vec<char>>();
  sorted_letters.sort();
  assert_eq!(sorted_letters.len(), kGameLen);
  let mut it = PermutationIt::new(main, sorted_letters);
  let mut game = Game::new(main);

  while let Some(key) = it.next() {
    if key.len() == kGameLen {
      if let Some(v) = words.letters_words.get(key) {
        game.reset(key, v);
      }
    }
    else if let Some(v) = words.letters_words.get(key) {
      game.add_sub_key(key, v);
    }
  }
  if !game.valid_generation() {
    //return Error::new(Code::INTERNAL, "no solution found");
  }
  return Ok(game);
}

fn from_words_once<'a>(conf: &config::Config, words: &'a anagram::Anagram, generator: &Generator)
    -> StatusOr<Game<'a>> {
  let mut game = Game::new(generator.main);
  let mut it = generator.iter();
  let mut w_count = 0;
  let mut skip = false;

  while let Some(key) = it.next() {
    if key.len() == kGameLen {
      if game.valid_generation() { return Ok(game); }

      if let Some(v) = words.letters_words.get(key) {
        w_count = v.len();
        game.reset(key, v);
        if w_count >= conf.min_pangram_count { continue; }
      }
      skip = true;
    }
    else if let Some(v) = words.letters_words.get(key) {
      w_count += v.len();
      game.add_sub_key(key, v);
      skip = w_count > conf.game_word_limit;
    }

    if skip {
      skip = false;
      if it.advance() { continue; }
      else { break; }
    }
  }
  return Error::new(Code::INTERNAL, "could not find enough words");
}

pub fn from_words<'a>(conf: &config::Config, words: &'a anagram::Anagram)
    -> StatusOr<Game<'a>> {
  let rng = &mut rand::rng();
  let main_candidates = words.letters_by_freq()
                             .into_iter()
                             .take(conf.mid_letter_min_rank)
                             .collect::<Vec<char>>();
  for tries in 1..10000 {
    let main = *main_candidates.choose(rng).unwrap();
    let generator = Generator::new(main);
    if let Ok(game) = from_words_once(conf, words, &generator) {
      return Ok(game);
    }
    if tries % 1000 == 0 { println!("tries = {tries}..."); }
  }
  return Error::new(Code::INTERNAL, "exhausted");
}

