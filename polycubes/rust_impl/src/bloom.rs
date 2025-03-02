use std::hash::{Hash, Hasher};
use rustc_hash::*;

use super::constants::*;
use super::utils::*;

type Bits768T = [u64; 12];
const BUCKET_COUNT:u64 = 3;
const BUCKET_BIT_LEN:u64 = 8;
const BUCKET_U64_LEN:u64 = 3;
const FRAGMENT_MASK:u64 = 2u64.pow(BUCKET_BIT_LEN as u32) - 1;

// Poor man's bloom filter. Fixed capacity.
pub struct Bloom {
  bitfield: Bits768T,
}

impl Bloom {
  pub fn new() -> Bloom {
    return Bloom{ bitfield: Bits768T::zeros(), };
  }
  pub fn contains(&mut self, pc: &[IdxT]) -> bool {
    let mut s = FxHasher::default();
    Hash::hash_slice(pc, &mut s);
    let h = s.finish();
    let mut have_it = true;
    for b in 0..BUCKET_COUNT {
      let fragment = (h >> (b * BUCKET_BIT_LEN)) & FRAGMENT_MASK;
      let to_write = 1u64 << (fragment % 64);
      let target = &mut self.bitfield[(b*BUCKET_U64_LEN + fragment/64) as usize];
      have_it &= (*target & to_write) > 0;
      *target |= to_write;
    }
    return have_it;
  }
  pub fn clear(&mut self) {
    self.bitfield = Bits768T::zeros();
  }
}

#[test]
fn bloom_contains_test() {
  const ROUNDS:u64 = 100;
  let mut collisions = 0;
  let mut bloom = Bloom::new();
  for r in 0..ROUNDS {
    for i in 0..(FRAGMENT_MASK/5) {
      let cube = [(r+i) as IdxT, (r+i+1) as IdxT];
      if bloom.contains(&cube) { collisions += 1; }
      assert!(bloom.contains(&cube));
    }
    bloom.clear();
  }
  assert!(collisions < ROUNDS/2, "{}", collisions);
}

