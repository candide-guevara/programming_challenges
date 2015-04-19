#ifndef COMMON_H_
#define COMMON_H_

#include <cstdint>
#include <cassert>
#include <sstream>
#include <exception>
#include <type_traits>

#include <logger.hpp>
#include <perfcounter.hpp>
using namespace std;

#ifndef __LEVEL_ASSERT__
  #define __LEVEL_ASSERT__ 1
#endif

#if __LEVEL_ASSERT__ > 1
  #define ASSERT(exp, msg) \
    if (!(exp)) { LOG_ASSERT(exp,msg); throw EGM::AssertFailedException(#msg); }
#elif __LEVEL_ASSERT__ > 0
  #define ASSERT(exp, msg) \
    if (!(exp)) { LOG_ASSERT(exp,msg); assert(exp); }
#else
  #define ASSERT(exp,msg)
#endif

namespace EGM {

/**
 * Thrown by assert macros when something fails. Should only be catched by main or in tests.
 */
class AssertFailedException : public exception {
  public:
    AssertFailedException(const string& msg) : _msg(msg) {}
    const char* what() const noexcept { return _msg.c_str(); }
  private:
    string _msg;
};

template <class T, class=void>
struct is_container : false_type {};
template <class T>
struct is_container<T, 
  typename enable_if<is_same<typename T::const_iterator, typename T::const_iterator>::value>::type> : true_type {};

template <class T, class=void>
struct has_to_string : false_type {};
template <class T>
struct has_to_string<T, 
  typename enable_if<&T::toString != 0>::type> : true_type {};

template <class T>
struct is_pair : false_type {};
template <class T, class U>
struct is_pair<pair<T,U> > : true_type {};

template <class T>
string toStrHelper (const T& iArg);
template <class T>
string toStrInnerHelper(const T& iArg, typename enable_if<has_to_string<T>::value,bool>::type);
template <class T>
string toStrInnerHelper(const T& iArg, typename enable_if<is_container<T>::value && !has_to_string<T>::value,bool>::type);
template <class T>
string toStrInnerHelper(const T& iArg, typename enable_if<is_scalar<T>::value,bool>::type);
template <class T>
string toStrInnerHelper(const T& iArg, typename enable_if<is_pair<T>::value,bool>::type);

template <class T>
string toStrInnerHelper(const T& iArg, typename enable_if<has_to_string<T>::value,bool>::type) {
  stringstream ss;
  ss << iArg.toString();
  return ss.str();
}
template <class T>
string toStrInnerHelper(const T& iArg, typename enable_if<is_container<T>::value && !has_to_string<T>::value,bool>::type) {
  stringstream ss;
  ss << "[";
  for (auto it=iArg.begin(); it != iArg.end(); ++it)
    ss << toStrHelper(*it) << ",";
  ss << "]";
  return ss.str();
}
template <class T>
string toStrInnerHelper(const T& iArg, typename enable_if<is_scalar<T>::value,bool>::type) {
  stringstream ss;
  ss << iArg;
  return ss.str();
}
template <class T>
string toStrInnerHelper(const T& iArg, typename enable_if<is_pair<T>::value,bool>::type) {
  stringstream ss;
  ss << "(" << toStrHelper(iArg.first) << "," << toStrHelper(iArg.second) << ")";
  return ss.str();
}

// Attempts to print everything
template <class T>
string toStrHelper (const T& iArg) {
  static_assert(
    is_scalar<T>::value || is_container<T>::value || has_to_string<T>::value || is_pair<T>::value,
    "Cannot print an object of this type");
  return toStrInnerHelper(iArg, true);
}

} // namespace EGM

#endif /* COMMON_H_ */
