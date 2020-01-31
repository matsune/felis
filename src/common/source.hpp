#ifndef SOURCE_HPP
#define SOURCE_HPP

#include <string>
#include <vector>
#include "pos.hpp"
#include "source.hpp"

using namespace std;

class Source {
  string& filename;
  vector<uint32_t> lineCols;

 public:
  Pos getPos(uint32_t offset);
  Source(string& filename) : filename(filename), lineCols({0}){};
  void line();
  void columns(int c = 1);
  const char* getFilename() { return filename.c_str(); };
};

#endif
