#ifndef FELIS_SYNTAX_RUNE_H_
#define FELIS_SYNTAX_RUNE_H_

#include <iostream>

namespace felis {

struct rune {
 public:
  uint32_t scalar;

  bool operator==(const int32_t &c) const { return scalar == c; }
  bool operator!=(const int32_t &c) const { return scalar != c; }
  bool operator==(const rune &r) const { return scalar == r.scalar; }

  rune(uint32_t scalar = 0) : scalar(scalar) {}

  int len_utf8();
  int encode_utf8(char[4]);

 private:
};

}  // namespace felis

#endif  // FELIS_SYNTAX_RUNE_H_
