#ifndef TOKEN_HPP
#define TOKEN_HPP
#include <string>
using namespace std;

enum Kind { IDENT = 258, NL = 259, SPACE = 260, LIT_INT = 261 };

class Token {
 public:
  int kind;
  bool nl;
  bool ws;
  uint32_t offset, len;

  string str;
  uint64_t num;

  Token() : kind(0), nl(false), ws(false), offset(0), len(0){};
};

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
