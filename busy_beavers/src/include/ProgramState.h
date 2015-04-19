/*
 * Created on: Nov 23, 2013
 *      Author: eguevara
 */

#ifndef PROGRAMSTATE_H_
#define PROGRAMSTATE_H_

#include "logging.h"
#include "config.h"

/**
 * Represents the state of the program at a precise value in time.
 */
class ProgramState {
  public:
    // Keep the default constructor empty to minimize object creation overhead
    ProgramState() {}
    ProgramState(const ProgramState& copy);

    bool hasStopped() const {
      ASSERT(data[IP] < MAX_CODE_LEN, "Instruction pointer out of range");
      return !data[data[IP]];
    }

    const uint8_t* getSerializedState() const {
      return data;
    }

    uint8_t getCode() const {
      ASSERT(data[IP] < MAX_CODE_LEN, "Instruction pointer out of range");
      return data[data[IP]];
    }
    uint8_t getCode(unsigned idx) const {
      ASSERT(idx < MAX_CODE_LEN, "Instruction pointer out of range");
      return data[idx];
    }
    void setCode(unsigned idx, uint8_t value) {
      ASSERT(idx < MAX_CODE_LEN && value < MAX_VALUE, "Cannot load value " << value << " at " << idx);
      data[idx] = value;
    }

    void loadCode(const uint8_t *start);
    void resetRegisters ();
    /** Prefer to use this only for test purposes */
    bool slowCompare (const ProgramState& iOther) const;

    uint8_t getR0() const {
      return data[R0];
    }
    void setR0(uint8_t r0) {
      ASSERT(r0 < MAX_VALUE, "Cannot load value " << r0 << " at R0");
      data[R0] = r0;
    }

    uint8_t getR1() const {
      return data[R1];
    }
    void setR1(uint8_t r1) {
      ASSERT(r1 < MAX_VALUE, "Cannot load value " << r1 << " at R1");
      data[R1] = r1;
    }

    uint8_t getIp() const {
      return data[IP];
    }
    void setIp(uint8_t ip) {
      ASSERT(ip < MAX_VALUE, "Cannot load value " << ip << " at IP");
      data[IP] = ip;
    }

  private:

    static const unsigned int R0 = 16, R1 = 17, IP = 18, DATA_LEN = 19;
    uint8_t data[20];
};

std::ostream& operator <<(std::ostream& os, const ProgramState& iState);

#endif /* PROGRAMSTATE_H_ */
