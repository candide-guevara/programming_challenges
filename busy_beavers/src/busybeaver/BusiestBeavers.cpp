/*
 * Created on: Nov 30, 2013
 *      Author: eguevara
 */

#include <BusiestBeavers.h>

void BusiestBeavers::pushCopy(const ProgramState& iProg, const ExeStats& iStat) {
  // We check first to avoid a copy
  if (iStat.stops && iStat.output >= weakestScore) {
    LOG_DEBUG("Adding to queue : " << iProg << " / " << iStat);
    emplace(iProg, iStat);

    if (maxSize < size())
      erase(cbegin());
    weakestScore = cbegin()->second.output;
    ASSERT(cbegin()->second.output <= busiest().second.output, "You messed up tails and heads");
  }
}

std::ostream& operator <<(std::ostream& oOs, const BusiestBeavers& iItem) {
  oOs << "BusiestBeavers (size=" << iItem.size() << std::endl;
  for (auto it = iItem.begin(); it != iItem.end(); ++it)
    oOs << "  " << it->first << " | " << it->second << std::endl;
  oOs << ") // BusiestBeavers";
  return oOs;
}
