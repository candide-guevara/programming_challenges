/*
 * Created on: Nov 25, 2013
 *      Author: eguevara
 */

#include "ProgramToSave.h"
#include <ProgramMap.h>

ProgramToSave::ProgramToSave() {
  clear();
}

ProgramToSave::ProgramToSave(const BigInteger isaveUpto, const BigInteger isaveDropRatio) {
  clear();
  setSaveDropRatio(isaveDropRatio);
  setSaveUpto(isaveUpto);
}

void ProgramToSave::clear() {
  // sweet constant time :-)
  head = tail = 0;
  saveIfNull = saveDropRatio;
}
BigInteger ProgramToSave::size() const {
  return (saveUpTo + tail - head) % saveUpTo;
}

void ProgramToSave::setSaveUpto(const BigInteger isaveUpto) {
  this->saveUpTo = isaveUpto + 1;
  toSave.clear();
  toSave.reserve(this->saveUpTo);
}
void ProgramToSave::setSaveDropRatio(const BigInteger isaveDropRatio) {
  this->saveDropRatio = isaveDropRatio;
  saveIfNull = isaveDropRatio;
  ASSERT(isaveDropRatio, "Save drop ratio is too low");
}

void ProgramToSave::absolutelySaveForLater(const ProgramState& iState, const ExeStats& iStats) {
  LOG_DEBUG("Execution state to save : " << iState << " / " << iStats);

  auto& pair = toSave[tail];
  pair.first = iState;
  pair.second = iStats;

  tail = (tail + 1) % saveUpTo;
  head = head != tail ? head : (head + 1) % saveUpTo;
  saveIfNull = saveDropRatio;
  ASSERT(head != tail, "You messed up the tail and head index in the circular buffer");
}
void ProgramToSave::maybeSaveForLater(const ProgramState& iState, const ExeStats& iStats) {
  if (--saveIfNull) return;
  absolutelySaveForLater(iState, iStats);
}

void ProgramToSave::saveIntoMap(ProgramMap& iMap, const ExeStats& iFinalStats) {
  LOG_DEBUG("Saving into past execution history " << size() << " states");
  LOG_DEBUG("If program halts, need to adjust the execution stats of saved states : " << iFinalStats);

  // TODO optim adding 1 by 1 can be improved with a bulk insert
  for (auto it = begin(), last = end(); it != last; ++it) {
    InnerItem item = *it;
    item.second.stops = iFinalStats.stops;
    item.second.reachedMaxIt = iFinalStats.reachedMaxIt;

    if (iFinalStats.stops) {
      item.second.iterations = iFinalStats.iterations - item.second.iterations;
      item.second.output = iFinalStats.output - item.second.output;
      item.second.stops = true;
    }
    else {
      item.second.reachedMaxIt = iFinalStats.reachedMaxIt;
    }
    iMap.addCopy(item.first, item.second);
  }
}

ProgramToSave::iterator ProgramToSave::begin() const {
  return ProgramToSave::iterator(head, this);
}
ProgramToSave::iterator ProgramToSave::end() const {
  return ProgramToSave::iterator(tail, this);
}

ProgramToSave::iterator& ProgramToSave::iterator::operator --() {
  idx = (data->saveUpTo + idx - 1) % data->saveUpTo;
  return *this;
}
ProgramToSave::iterator& ProgramToSave::iterator::operator ++() {
  idx = (idx + 1) % data->saveUpTo;
  return *this;
}
bool ProgramToSave::iterator::operator ==(const iterator& iOther) {
  return idx == iOther.idx;
}
bool ProgramToSave::iterator::operator !=(const iterator& iOther) {
  return !(*this == iOther);
}
const std::pair<ProgramState, ExeStats>& ProgramToSave::iterator::operator *() const {
  return data->toSave[idx];
}

std::ostream& operator <<(std::ostream& oOs, const ProgramToSave& iItem) {
  oOs << "ProgramToSave (size=" << iItem.size() << ", save_drop=" << iItem.saveDropRatio
      << ", drop=" << iItem.saveIfNull << ", max_size=" << iItem.saveUpTo
      << ", head=" << iItem.head << ", tail=" << iItem.tail << ")" << std::endl;

  oOs << "Last saved elements : " << std::endl;
  auto it = iItem.end(), begin = iItem.begin();
  for (unsigned count = 0; count < 10 && it != begin; ++count) {
    --it;
    oOs << "  " << (*it).first << " / " << (*it).second << std::endl;
  }
  return oOs;
}
