/*
 * Created on: Dec 1, 2013
 *      Author: eguevara
 */

#include "BigDecimal.h"

BigInteger bigIntPow(BigInteger ioBase, uint32_t iExp) {
  BigInteger result = 1;
  for (uint32_t exp = 0; exp < iExp; ++exp)
    result *= ioBase;
  return result;
}
