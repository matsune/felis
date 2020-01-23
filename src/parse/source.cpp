#include "source.hpp"

Pos Source::getPos(uint32_t offset) {
  Pos pos;
  for (int i = 0; i < lineCols.size(); ++i) {
    auto lineCol = lineCols[i];
    if (offset < lineCol) {
      pos.columns(offset);
      offset = 0;
      break;
    } else {
      if (i < lineCols.size() - 1) {
        pos.lines();
        offset -= lineCol;
      } else {
        pos.columns(offset);
        offset -= lineCol;
        break;
      }
    }
  }
  pos.columns(offset);
  return pos;
}

void Source::line() { lineCols.push_back(0); };

void Source::columns(int c) { lineCols.back() += c; };
