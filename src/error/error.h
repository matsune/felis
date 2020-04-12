#ifndef FELIS_ERROR_ERROR_H_
#define FELIS_ERROR_ERROR_H_

#include <exception>
#include <sstream>
#include <string>

#include "string/string.h"
#include "syntax/pos.h"

namespace felis

{

class CompileError : public std::exception {
 public:
  CompileError(std::string msg) : msg_(msg){};
  CompileError(Pos pos, std::string msg) {
    std::ostringstream ss;
    ss << pos.line << ":" << pos.column << " " << msg;
    msg_ = ss.str();
  };

  template <typename... Args>
  static CompileError CreatePosFmt(Pos pos, const std::string& fmt,
                                   Args... args) {
    return CompileError(pos, format(fmt, args...));
  };

  const char* what() const throw() { return msg_.c_str(); }

 protected:
  std::string msg_;
};

}  // namespace felis

#endif  // FELIS_ERROR_ERROR_H_
