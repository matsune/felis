#include "error.hpp"
#include <iostream>

using namespace std;

bool ErrorHandler::report() {
  for (auto& err : errors) {
    cerr << filename << ":" << err->pos.line + 1 << ":" << err->pos.column + 1
         << " " << err->msg << endl;
  }
  return !errors.empty();
};
