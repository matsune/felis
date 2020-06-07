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

  std::shared_ptr<Ty> val;

  static StmtResult NonValue(std::shared_ptr<Ty> val = nullptr) {
    return StmtResult(Kind::NON_VALUE, val);
  }

  static StmtResult Ret(std::shared_ptr<Ty> val = nullptr) {
    return StmtResult(Kind::RET, val);
  }

  static StmtResult Expr(std::shared_ptr<Ty> val) {
    return StmtResult(Kind::EXPR, val);
  }

  StmtResult(StmtResult::Kind kind = StmtResult::Kind::NON_VALUE,
             std::shared_ptr<Ty> val = nullptr)
      : kind_(kind), val(val) {}

  bool IsNonValue() const { return kind_ == StmtResult::Kind::NON_VALUE; }
  bool IsRet() const { return kind_ == StmtResult::Kind::RET; }
  bool IsExpr() const { return kind_ == StmtResult::Kind::EXPR; }

 private:
  StmtResult::Kind kind_;
};

}  // namespace felis

#endif  // FELIS_CHECK_STMT_RESULT_H_
