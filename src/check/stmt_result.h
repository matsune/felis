#ifndef FELIS_CHECK_STMT_RESULT_H_
#define FELIS_CHECK_STMT_RESULT_H_

#include <memory>

#include "check/ty.h"

namespace felis {

class StmtResult {
 public:
  // NON_VALUE: assign, var/let statement
  // RET: ret statement
  // EXPR: expr statements
  enum Kind { NON_VALUE, RET, EXPR };

  std::shared_ptr<Ty> type;

  static StmtResult NonValue() {
    return StmtResult(Kind::NON_VALUE, nullptr);
  }

  static StmtResult Ret() {
    return StmtResult(Kind::RET, nullptr);
  }

  static StmtResult Expr(std::shared_ptr<Ty> type) {
    return StmtResult(Kind::EXPR, type);
  }

  StmtResult(StmtResult::Kind kind = StmtResult::Kind::NON_VALUE,
             std::shared_ptr<Ty> type = nullptr)
      : kind_(kind), type(type) {}

  bool IsNonValue() const { return kind_ == StmtResult::Kind::NON_VALUE; }
  bool IsRet() const { return kind_ == StmtResult::Kind::RET; }
  bool IsExpr() const { return kind_ == StmtResult::Kind::EXPR; }

 private:
  StmtResult::Kind kind_;
};

}  // namespace felis

#endif  // FELIS_CHECK_STMT_RESULT_H_
