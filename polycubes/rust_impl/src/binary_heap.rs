use super::constants::*;
use super::utils::*;

const HEAP_LEN:usize = 2usize.pow(4);
type TreeT = [IdxT; HEAP_LEN];

// The alignment to 8 bytes and the order of the fields makes it possible to cast the whole pointer to u64
// and perform comparisons on a single integer.
#[cfg(feature="heap_funky")]
#[derive(Debug, Copy, Clone)]
#[repr(C, align(8))]
struct Pointer {
  idx: u32,
  lvl: u32,
}

#[cfg(not(feature="heap_funky"))]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Copy, Clone)]
struct Pointer {
  lvl: u32,
  idx: u32,
}

pub struct BinHeap {
  tail: Pointer,
  tree: TreeT,
}

impl Pointer {
  #[inline(always)]
  fn new() -> Pointer {
    return Pointer{ lvl:1, idx:0, };
  }
  #[inline(always)]
  fn as_u64(&self) -> *const u64 {
    return std::ptr::addr_of!(*self) as *const u64;
  }
  #[inline(always)]
  fn is_origin(&self) -> bool {
    #[cfg(feature="heap_funky")] {
      return unsafe { *self.as_u64() == 1 << 32 };
    }
    #[cfg(not(feature="heap_funky"))] {
      return *self == Pointer::new();
    }
  }
  #[inline(always)]
  fn lt(&self, rhs: &Pointer) -> bool {
    #[cfg(feature="heap_funky")] {
      return unsafe { *self.as_u64() < *rhs.as_u64() };
    }
    #[cfg(not(feature="heap_funky"))] {
      return self < rhs;
    }
  }
  #[inline(always)]
  fn lin(&self) -> usize {
    return (self.lvl - 1 + self.idx) as usize;
  }
  #[inline(always)]
  fn advance(&mut self) {
    if self.lvl-1 == self.idx {
      self.lvl = self.lvl << 1;
      self.idx = 0;
    }
    else { self.idx += 1; }
  }
  #[inline(always)]
  fn retreat(&mut self) {
    if self.idx == 0 {
      self.lvl = self.lvl >> 1;
      self.idx = self.lvl - 1;
    }
    else { self.idx -= 1; }
  }
  #[inline(always)]
  fn parent(&self) -> Pointer {
    let mut par = *self;
    if self.is_origin() { return par; }
    par.lvl = par.lvl >> 1;
    par.idx = par.idx >> 1;
    return par;
  }
  #[inline(always)]
  fn children(&self) -> (Pointer,Pointer) {
    let c1 = Pointer {
      lvl: self.lvl << 1,
      idx: self.idx << 1,
    };
    let mut c2 = c1;
    c2.idx += 1;
    return (c1,c2);
  }
}

impl BinHeap {
  pub fn new() -> BinHeap {
    debug_assert!(CUBE_ARR_PAD < HEAP_LEN);
    return BinHeap {
      tree: TreeT::zeros(),
      tail: Pointer::new(),
    };
  }
  pub fn clear(&mut self) { self.tail = Pointer::new(); }

  pub fn to_string(&self) -> String {
    return format!("tree = {:?}, tail = {:?}",
                   &self.tree[0..self.tail.lin()], self.tail);
  }

  pub fn push(&mut self, v: IdxT) {
    let mut p_chd = self.tail;
    self.tree[p_chd.lin()] = v;
    self.tail.advance();
    loop {
      let p_par = p_chd.parent();
      let v_chd = self.tree[p_chd.lin()];
      let v_par = self.tree[p_par.lin()];
      if v_chd <= v_par { break; }
      self.tree[p_chd.lin()] = v_par;
      self.tree[p_par.lin()] = v_chd;
      p_chd = p_par;
    }
  }
  pub fn pop(&mut self) -> IdxT {
    let mut par = Pointer::new();
    let ret = self.tree[0];

    self.tail.retreat();
    self.tree[0] = self.tree[self.tail.lin()];

    let (mut c1, mut c2) = par.children();
    while c1.lt(&self.tail) {
      let vp = self.tree[par.lin()];
      let v1 = self.tree[c1.lin()];
      if c2.lt(&self.tail) {
        let v2 = self.tree[c2.lin()];
        if v1 < v2 {
          if vp >= v2 { break; }
          self.tree[par.lin()] = v2;
          par = c2;
        }
        else {
          if vp >= v1 { break; }
          self.tree[par.lin()] = v1;
          par = c1;
        }
        self.tree[par.lin()] = vp;
        (c1, c2) = par.children();
        continue;
      }
      else if vp < v1 {
        self.tree[par.lin()] = v1;
        self.tree[c1.lin()] = vp;
      }
      break;
    }
    return ret;
  }
}

#[test]
fn bin_heap_push_pop_test() {
  let mut heap = BinHeap::new();
  heap.push(3);
  heap.push(2);
  heap.push(4);
  heap.push(1);
  heap.push(0);
  assert_eq!(heap.pop(), 4 as IdxT);
  assert_eq!(heap.pop(), 3 as IdxT);
  assert_eq!(heap.pop(), 2 as IdxT);
  assert_eq!(heap.pop(), 1 as IdxT);
  assert_eq!(heap.pop(), 0 as IdxT);
  assert!(heap.tail.is_origin());
}

#[test]
fn bin_heap_push_pop_dupes_test() {
  let mut heap = BinHeap::new();
  heap.push(2);
  heap.push(1);
  heap.push(3);
  heap.push(2);
  assert_eq!(heap.pop(), 3 as IdxT);
  assert_eq!(heap.pop(), 2 as IdxT);
  assert_eq!(heap.pop(), 2 as IdxT);
  assert_eq!(heap.pop(), 1 as IdxT);
  assert!(heap.tail.is_origin());
}

#[test]
fn bin_heap_clear_test() {
  let mut heap = BinHeap::new();
  heap.push(3);
  heap.push(1);
  heap.push(3);
  heap.clear();
  heap.push(2);
  assert_eq!(heap.pop(), 2 as IdxT);
  assert!(heap.tail.is_origin());
}

