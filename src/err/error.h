#ifndef FELIS_ERR_ERROR_H_
#define FELIS_ERR_ERROR_H_
#include <sstream>
#include <string>

#include "syntax/pos.h"

namespace felis

{

class Error {
 public:
  Error(const Error&) = delete;

  Error(Error&&) = default;

  Error() = default;
  ~Error() = default;

  virtual std::string what() = 0;
};

class PosError : Error {
 public:
  PosError(Pos pos, std::string msg) : pos_(pos), msg_(msg){};

  std::string what() override {
    std::ostringstream ss;
    ss << pos_.line << ":" << pos_.column << " " << msg_;
    return ss.str();
  }

 protected:
  Pos pos_;
  std::string msg_;
};

class StrError : Error {
 public:
  StrError(std::string str) : str_(str){};

  std::string what() override { return str_; }

 private:
  std::string str_;
};

}  // namespace felis

#endif  // FELIS_ERR_ERROR_H_
