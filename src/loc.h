#ifndef FELIS_LOC_H_
#define FELIS_LOC_H_

#include <fstream>
#include <iostream>

namespace felis {

using Loc = uint32_t;

struct Pos {
  uint32_t line, col;

  Pos(uint32_t line = 1, uint32_t col = 1) : line(line), col(col) {}

  Pos(std::ifstream &in, Loc loc) : Pos() {
    while (loc-- > 0) {
      if (in.eof() || in.fail()) {
        break;
      }
      char c = in.get();
      if (c == '\n') {
        line++;
        col = 1;
      } else {
        col++;
      }
    }
  }
};

}  // namespace felis

#endif  // FELIS_LOC_H_
