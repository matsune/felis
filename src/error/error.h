#ifndef FELIS_ERROR_ERROR_H_
#define FELIS_ERROR_ERROR_H_

#include <exception>
#include <sstream>
#include <string>

#include "loc.h"
#include "string/string.h"

namespace felis {

class CompileError : public std::exception {
 public:
  CompileError(std::string msg) : msg(msg){};

  template <typename... Args>
  static CompileError Create(const std::string &fmt, Args... args) {
    return CompileError(format(fmt, args...));
  };

  const char *what() const throw() { return msg.c_str(); }

 protected:
  std::string msg;
};

class LocError : public CompileError {
 public:
  LocError(Loc loc, std::string msg) : CompileError(msg), loc(loc) {}

  template <typename... Args>
  static LocError Create(Loc loc, const std::string &fmt, Args... args) {
    return LocError(loc, format(fmt, args...));
  };

  Loc loc;
};

}  // namespace felis

#endif  // FELIS_ERROR_ERROR_H_
