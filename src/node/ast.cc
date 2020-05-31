#include "ast.h"

namespace felis {

namespace ast {

bool Stmt::IsLastStmt() const {
  if (parent) {
    assert(!parent->stmts.empty());
    return parent->stmts.back().get() == this;
  } else {
    // fn block
    return true;
  }
}

bool Expr::IsBlockRet() const {
  if (!parent) return !as_stmt;
  return !parent->as_stmt && IsLastStmt() && as_stmt;
}

}  // namespace ast

}  // namespace felis
