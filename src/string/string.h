#ifndef FELIS_STRING_STRING_H_
#define FELIS_STRING_STRING_H_

#include <cstdio>
#include <string>
#include <vector>

namespace felis {

template <typename... Args>
std::string format(const std::string& fmt, Args... args) {
  size_t len = std::snprintf(nullptr, 0, fmt.c_str(), args...);
  std::vector<char> buf(len + 1);
  std::snprintf(&buf[0], len + 1, fmt.c_str(), args...);
  return std::string(&buf[0], &buf[0] + len);
}

}  // namespace felis

#endif  // FELIS_STRING_STRING_H_
