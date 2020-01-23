#ifndef SOURCE_HPP
#define SOURCE_HPP

#include <string>
#include <vector>

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

class Loc {
 public:
  Pos begin, end;

  Loc(){};
  Loc(Pos &begin, Pos &end) : begin(begin), end(end){};

  void step() { begin = end; }
  void columns(int col = 1) { end.columns(col); }
  void lines(int ln = 1) { end.lines(ln); }
};

class Source {
  string filename;
  vector<uint32_t> lineCols;

 public:
  Pos getPos(uint32_t offset);
  Source(string filename = "") : filename(filename), lineCols({0}){};
  void line();
  void columns(int c = 1);
};

#endif
