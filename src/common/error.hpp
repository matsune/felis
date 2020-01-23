#ifndef ERROR_HPP
#define ERROR_HPP

#include <memory>
#include <string>
#include <vector>
#include "source.hpp"

using namespace std;

class Error {
 public:
  string msg;
  Pos pos;

  Error(string msg, Pos pos) : msg(msg), pos(pos){};
};

class ErrorHandler {
 public:
  string &filename;
  vector<unique_ptr<Error>> errors;
  bool report();

  ErrorHandler(string &filename) : filename(filename){};
};

#endif
