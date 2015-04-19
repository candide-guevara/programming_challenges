/*
 * Created on: Nov 23, 2013
 *      Author: eguevara
 */

#include <ProgramState.h>
#include <cstring>

ProgramState::ProgramState(const ProgramState& copy) {
  std::memcpy(data, copy.getSerializedState(), DATA_LEN);
  // TODO optim : dirty pointer reinterpretation
  /*const uint64_t *source = reinterpret_cast<const uint64_t*>(copy.getSerializedState());
   uint64_t *dest = reinterpret_cast<uint64_t*>(data);
   dest[0] = source[0];
   dest[1] = source[1];
   dest[2] = source[2];*/
}

void ProgramState::loadCode(const uint8_t *start) {
  std::memcpy(data, start, MAX_CODE_LEN);
}

void ProgramState::resetRegisters() {
  const static uint32_t eraser = 0;
  // Low level trick to take advantage of register processor arch, is it really worth ?
  uint32_t *registers = reinterpret_cast<uint32_t*>(data + R0);
  *registers = eraser;
}

bool ProgramState::slowCompare(const ProgramState& iOther) const {
  bool result = true;
  for (unsigned idx = 0; idx < MAX_CODE_LEN; ++idx)
    if (getCode(idx) != iOther.getCode(idx))
      result = false;
  if (getR0() != iOther.getR0())
    result = false;
  if (getR1() != iOther.getR1())
    result = false;
  if (getIp() != iOther.getIp())
    result = false;
  return result;
}

std::ostream& operator <<(std::ostream& oOs, const ProgramState& iState) {
  oOs << "ProgramState (code=[ " << std::hex;
  for (unsigned i = 0; i < MAX_CODE_LEN; ++i)
    oOs << (int) iState.getCode(i) << " ";
  oOs << "], RO=" << (int) iState.getR0();
  oOs << ", R1=" << (int) iState.getR1();
  oOs << ", IP=" << (int) iState.getIp() << ")" << std::dec;
  return oOs;
}
