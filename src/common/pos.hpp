#ifndef POS_HPP
#define POS_HPP

#include <string>

using namespace std;

class Pos {
 public:
  unsigned int line, column;

  Pos(int line = 1, int column = 1) : line(line), column(column){};

  void lines(int ln = 1) {
    line += ln;
    column = 1;
  }
  void columns(int col = 1) { column += col; }
};

#endif
