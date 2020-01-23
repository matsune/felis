#ifndef LOC_HPP
#define LOC_HPP

#include "pos.hpp"

class Loc {
 public:
  Pos begin, end;

  Loc(){};
  Loc(Pos &begin, Pos &end) : begin(begin), end(end){};

  void step() { begin = end; }
  void columns(int col = 1) { end.columns(col); }
  void lines(int ln = 1) { end.lines(ln); }
};

#endif
