#ifndef FELIS_ANALYSIS_TY_INFERER_H_
#define FELIS_ANALYSIS_TY_INFERER_H_

#include <iostream>

#include "def_table.h"
#include "error/handler.h"
#include "syntax/ast.h"

namespace felis {

class TyInferer {
 public:
  TyInferer(ErrorHandler &handler)
      : handler_(handler), defTable_(DefTable(handler)) {}
  void Parse(std::unique_ptr<File> &file);

  // debug
  void PrintTable();

 private:
  ErrorHandler &handler_;
  Scope scope_ = 0;
  DefTable defTable_;

  template <typename... Args>
  void Raise(Pos pos, const std::string &fmt, Args... args);
};

}  // namespace felis

#endif  // FELIS_ANALYSIS_TY_INFERER_H_

