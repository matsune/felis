#ifndef FELIS_ERROR_HANDLER_H_
#define FELIS_ERROR_HANDLER_H_

#include <iostream>
#include <memory>
#include <string>
#include <vector>

#include "syntax/pos.h"

namespace felis

{

struct Error {
  Pos pos;
  std::string message;

  Error(Pos pos = Pos(), std::string message = "")
      : pos(pos), message(message) {}
};

class ErrorHandler {
 public:
  ErrorHandler(std::string filename = "") : filename_(filename){};
  ErrorHandler(const ErrorHandler&) = delete;
  ErrorHandler(ErrorHandler&& o) noexcept
      : errors_(move(o.errors_)), filename_(move(o.filename_)){};

  void SetFilename(std::string filename) { filename_ = filename; };

  void Raise(Pos pos, std::string message);

  bool HasError() { return !errors_.empty(); }

  void Report(std::ostream& = std::cerr);

 private:
  std::vector<std::unique_ptr<Error>> errors_;
  std::string filename_;
};

}  // namespace felis

#endif  // FELIS_ERROR_HANDLER_H_
