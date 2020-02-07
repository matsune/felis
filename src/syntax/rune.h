#ifndef FELIS_SYNTAX_RUNE_H_
#define FELIS_SYNTAX_RUNE_H_

#include <iostream>

namespace felis {

struct rune {
  uint32_t scalar;

  bool operator==(const int32_t &c) const { return int64_t(scalar) == c; }
  bool operator!=(const int32_t &c) const { return int64_t(scalar) != c; }
  bool operator==(const rune &r) const { return scalar == r.scalar; }

  explicit rune(uint32_t scalar = 0) : scalar(scalar) {}

  int len_utf8();
  int encode_utf8(char[4]);
};

}  // namespace felis

#endif  // FELIS_SYNTAX_RUNE_H_
