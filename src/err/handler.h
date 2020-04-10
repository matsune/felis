#ifndef FELIS_ERR_HANDLER_H_
#define FELIS_ERR_HANDLER_H_

#include <iostream>
#include <memory>
#include <string>
#include <vector>

#include "error.h"

namespace felis

{

class ErrorHandler2 {
 public:
  ErrorHandler2(std::string filename = "") : filename_(filename){};
  ErrorHandler2(const ErrorHandler2&) = delete;
  ErrorHandler2(ErrorHandler2&&) = default;

  void SetFilename(std::string filename) { filename_ = std::move(filename); }

  /* void Raise(std::unique_ptr<Error2> err) {
   * errors_.push_back(std::move(err)); } */
  /* void Raise(std::string message); */
  void Push(std::unique_ptr<Error2> err) { errors_.push_back(std::move(err)); }

  bool HasError() { return !errors_.empty(); }

  void Report(std::ostream& out = std::cerr) {
    for (auto& error : errors_) {
      out << "felisc error: " << filename_;

      out << error->what() << std::endl;
      /* if (error->pos) { */
      /*   out << ":" << error->pos->line << ":" << error->pos->column; */
      /* } */
      /* out << ": " << error->message << std::endl; */
    }
  }

 private:
  std::vector<std::unique_ptr<Error2>> errors_;
  std::string filename_;
};

}  // namespace felis

#endif  // FELIS_ERR_HANDLER_H_
