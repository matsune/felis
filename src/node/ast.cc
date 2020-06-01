#include "ast.h"

namespace felis {

namespace ast {

Stmt::Result Stmt::StmtResult() const {
  if (!parent) std::cout << "No parent " << this << std::endl;
  assert(parent);
  if (auto p = parent->As<FnDecl>()) {
    // function body block
    //
    // if the function has a ret type, last statement of this block will be a
    // ret value
    return p->proto->ret ? Stmt::Result::RET_VALUE : Stmt::Result::DISCARD;
  }
  if (auto p = parent->As<Block>()) {
    // as stmt
    bool is_last = p->stmts.back().get() == this;
    // if this statement is a last of parent block, derives parent result
    return is_last ? p->StmtResult() : Stmt::Result::DISCARD;
  }
  if (auto p = parent->As<If>()) {
    // if block
    return p->StmtResult();
  }
  if (As<RetStmt>()) return Stmt::Result::RET_VALUE;
  return Stmt::Result::EXPR_VALUE;
}

}  // namespace ast

}  // namespace felis
