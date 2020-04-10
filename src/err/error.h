#ifndef FELIS_ERR_ERROR_H_
#define FELIS_ERR_ERROR_H_
#include <sstream>
#include <string>

#include "syntax/pos.h"

namespace felis

{

class Error2 {
 public:
  Error2(const Error2&) = delete;

  Error2(Error2&&) = default;

  Error2() = default;
  ~Error2() = default;

  virtual std::string what() = 0;
};

class PosError : Error2 {
 public:
  PosError(Pos pos, std::string msg) : pos_(pos), msg_(msg){};

  std::string what() {
    std::ostringstream ss;
    ss << pos_.line << ":" << pos_.column << " " << msg_;
    return ss.str();
  }

 protected:
  Pos pos_;
  std::string msg_;
};

}  // namespace felis

#endif  // FELIS_ERR_ERROR_H_
