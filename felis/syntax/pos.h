#ifndef FELIS_SYNTAX_POS_H_
#define FELIS_SYNTAX_POS_H_

#include <iostream>

namespace felis

{

struct Pos {
  uint16_t line, column;

  Pos(int line = 1, int column = 1) : line(line), column(column) {}

  void Lines(int ln = 1) {
    line += ln;
    column = 1;
  }
  void Columns(int col = 1) { column += col; }
};

}  // namespace felis

#endif  // FELIS_SYNTAX_POS_H_
