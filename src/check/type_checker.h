#ifndef FELIS_CHECK_TYPE_CHECKER_H_
#define FELIS_CHECK_TYPE_CHECKER_H_

#include <map>
#include <memory>

#include "check/ctx.h"
#include "check/decl_checker.h"
#include "check/eval.h"
#include "node/ast.h"

namespace felis {

class TypeChecker {
 public:
  TypeChecker(TypeCheckCtx &ctx) : ctx_(ctx), decl_ck_(ctx.Is32bit()) {}

  void Check(const std::unique_ptr<ast::File> &);

 private:
  TypeCheckCtx &ctx_;
  DeclChecker decl_ck_;
  std::shared_ptr<Type> current_func_;

  Eval CheckBlock(const ast::Block *, bool needs_type, bool open_scope = true);
  Eval CheckStmt(const ast::AstNode *, bool needs_type);
  Eval CheckRet(const ast::RetStmt *);
  Eval CheckVarDecl(const ast::VarDeclStmt *);
  Eval CheckAssign(const ast::AssignStmt *);
  Eval CheckExpr(const ast::AstNode *, bool needs_type);
  Eval CheckLit(const ast::Literal *);
  Eval CheckIdent(const ast::Ident *);
  Eval CheckBinary(const ast::Binary *);
  Eval CheckUnary(const ast::Unary *);
  Eval CheckCall(const ast::Call *);
  Eval CheckArray(const ast::Array *);
  Eval CheckIndex(const ast::Index *);
  Eval CheckIf(const ast::If *, bool needs_type);
};

}  // namespace felis

#endif  // FELIS_CHECK_TYPE_CHECKER_H_
