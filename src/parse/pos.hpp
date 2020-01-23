#ifndef POS_HPP
#define POS_HPP

#include <string>

using namespace std;

class Pos {
 public:
  unsigned int line, column;

  Pos(int line = 0, int column = 0) : line(line), column(column){};

  void lines(int ln = 1) {
    line += ln;
    column = 0;
  }
  void columns(int col = 1) { column += col; }
};

#endif
