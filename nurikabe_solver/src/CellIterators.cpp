#include<sstream>
#include<CellIterators.hpp>

namespace EGM {

template<>
const int_fast32_t ConnectedIt::TRANSFORMATIONS[][2] = {
  { 0, 0 }, // first one is never used
  { 0, -1 },
  { 1, 1 },
  { -1, 1 },
  { -1, -1 }
};
template<>
const uint_fast32_t ConnectedIt::TRANSFORMATIONS_LEN = 5;

template<>
const int_fast32_t PoolIt::TRANSFORMATIONS[][2] = {
  { 0, 0 }, // first one is never used
  { 1, 0 },
  { 0, 1 },
  { -1, 0 }
};
template<>
const uint_fast32_t PoolIt::TRANSFORMATIONS_LEN = 4;

template<>
const int_fast32_t AdjacentIt::TRANSFORMATIONS[][2] = {
  { 0, 0 }, // first one is never used
  { 0, -1 },
  { 1, 0 },
  { 0, 1 },
  { 0, 1 },
  { -1, 0 },
  { -1, 0 },
  { 0, -1 },
  { 0, -1 }
};
template<>
const uint_fast32_t AdjacentIt::TRANSFORMATIONS_LEN = 9;

} // namespace EGM

