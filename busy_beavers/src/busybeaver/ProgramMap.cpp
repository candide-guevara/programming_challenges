/*
 * Created on: Nov 23, 2013
 *      Author: eguevara
 */

#include <ProgramMap.h>

// TODO optim try to play with hashing algo
size_t ProgramMap::StateHash::operator()(const ProgramState& iState) const {
  const uint64_t *code = reinterpret_cast<const uint64_t*>(iState.getSerializedState());
  size_t hash = *code + ((code[1] + 29) << 4);

  // The prime added to the sum really boost the quality of the hash
  size_t factor = (iState.getR0() << 17) + (iState.getR1() << 13)
    + iState.getIp() + 1021;
  return hash * factor;
}

bool ProgramMap::StateEquals::operator ()(const ProgramState& iStateL, const ProgramState& iStateR) const {
  const uint64_t *codeL = reinterpret_cast<const uint64_t*>(iStateL.getSerializedState());
  const uint64_t *codeR = reinterpret_cast<const uint64_t*>(iStateR.getSerializedState());
  uint64_t serializedCodeL = *codeL + (*(codeL + 1) << 4);
  uint64_t serializedCodeR = *codeR + (*(codeR + 1) << 4);

  if (serializedCodeL == serializedCodeR)
    if (iStateL.getR0() == iStateR.getR0() && iStateL.getR1() == iStateR.getR1() && iStateL.getIp() == iStateR.getIp())
      return true;
  return false;
}

ProgramMap::ProgramMap() {
  ASSERT(sizeof(size_t) >= 8, "This class does not support hashes less than 8 bytes");
  ASSERT(MAX_CODE_LEN == 16, "Hash function is not adapted to this code length");
  maxLoadFactor = 0.0l;
}

void ProgramMap::maxSizeHint(const BigInteger imaxSize) {
  ASSERT(states.empty(), "This method should be called before any element is inserted");
  ASSERT(imaxSize < states.max_size(), "STL container cannot handle max size " << imaxSize);
  states.reserve(imaxSize);
}

void ProgramMap::clear() {
  // TODO optim : write a custom allocator to quicly free/allocate all memory
  // slab allocator would be good since only 2 fixed size objects to store
  TAKE_MAX_PERF(maxLoadFactor, states.load_factor());
  states.clear();
}
BigInteger ProgramMap::size() const {
  // hopefully the size method on STL container is in O(1)
  return states.size();
}

void ProgramMap::addCopy(const ProgramState& iKey) {
  // Since states is a stl map the [] operator automatically adds the key if not found
  states[iKey];
}
void ProgramMap::addCopy(const ProgramState& iKey, const ExeStats& iValue) {
  states[iKey] = iValue;
}

bool ProgramMap::contains(const ProgramState& iKey) {
  return states.count(iKey);
}
ExeStats* ProgramMap::get(const ProgramState& iKey) {
  auto it = states.find(iKey);
  if (it != states.end()) {
    // you get a pointer to the element inside the collection, be careful
    return &(it->second);
  }
  return NULL;
}

ProgramMap::InnerMap::const_iterator ProgramMap::begin() {
  return states.begin();
}
ProgramMap::InnerMap::const_iterator ProgramMap::end() {
  return states.end();
}

std::string ProgramMap::perfInformation() const {
  std::ostringstream os;
  BigInteger maxBucket = 0, maxBucketCount = 0;
  BigInteger buckets = states.bucket_count();

  for (unsigned bucket = 0; bucket < buckets; ++bucket) {
    if (maxBucket < states.bucket_size(bucket)) {
      maxBucket = states.bucket_size(bucket);
      maxBucketCount = 1;
    }
    else if (maxBucket == states.bucket_size(bucket))
      maxBucketCount++;
  }

  os << "ProgramMap perf (size=" << states.size()
      << ", load_factor=" << states.load_factor() << ", max_lf=" << maxLoadFactor
      << ", max_bucket=" << maxBucket << ", max_count=" << maxBucketCount << ")";
  return os.str();
}

std::ostream& operator <<(std::ostream& oOs, const ProgramMap& iMap) {
  const static unsigned printCount = 10;
  auto& states = iMap.states;

  oOs << "ProgramMap (size=" << states.size()
      << ", load_factor=" << states.load_factor() << ")" << std::endl;

  oOs << "First " << printCount << " elements : " << std::endl;
  unsigned counter = 0;
  for (auto it = states.begin(); counter < printCount && it != states.end(); ++counter, ++it)
    oOs << "  " << it->first << " / " << it->second << std::endl;
  return oOs;
}
