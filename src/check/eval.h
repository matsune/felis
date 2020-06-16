#ifndef FELIS_CHECK_EVAL_H_
#define FELIS_CHECK_EVAL_H_

#include <memory>

#include "check/type.h"

namespace felis {

class Eval {
 public:
  // STMT: assign, var/let statements
  // TERM: statements containing termination
  // EXPR: expression
  enum Kind { STMT, TERM, EXPR };

  std::shared_ptr<Type> type;

  static Eval Stmt() { return Eval(Kind::STMT, nullptr); }

  static Eval Ret() { return Eval(Kind::TERM, nullptr); }

  static Eval Expr(std::shared_ptr<Type> type) {
    return Eval(Kind::EXPR, type);
  }

  Eval(Eval::Kind kind = Eval::Kind::STMT, std::shared_ptr<Type> type = nullptr)
      : kind_(kind), type(type) {}

  bool IsStmt() const { return kind_ == Eval::Kind::STMT; }
  bool IsRet() const { return kind_ == Eval::Kind::TERM; }
  bool IsExpr() const { return kind_ == Eval::Kind::EXPR; }

 private:
  Eval::Kind kind_;
};

}  // namespace felis

#endif  // FELIS_CHECK_EVAL_H_
