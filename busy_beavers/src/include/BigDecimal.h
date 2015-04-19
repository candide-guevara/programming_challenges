/*
 * Created on: Dec 1, 2013
 *      Author: eguevara
 */

#ifndef BIGDECIMAL_H_
#define BIGDECIMAL_H_

#include <cstdint>

// In case we need something bigger it will be easier to refactor thanks to the typedef
typedef uint64_t BigInteger;
static const BigInteger MAX_BIG_INT = UINT64_MAX;

/** SLOW do not use heavely */
BigInteger bigIntPow(BigInteger, uint32_t);

#endif /* BIGDECIMAL_H_ */
