#ifndef FELIS_SYNTAX_POS_H_
#define FELIS_SYNTAX_POS_H_

#include <iostream>

namespace felis

{

struct Pos {
  uint16_t line, column;

  Pos(uint16_t line = 1, uint16_t column = 1) : line(line), column(column) {}

  void Lines(uint16_t ln = 1) {
    line += ln;
    column = 1;
  }
  void Columns(uint16_t col = 1) { column += col; }
};

}  // namespace felis

#endif  // FELIS_SYNTAX_POS_H_
