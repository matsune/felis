#ifndef FELIS_CHECK_TYPE_CHECKER_H_
#define FELIS_CHECK_TYPE_CHECKER_H_

#include <map>
#include <memory>

#include "check/ctx.h"
#include "check/decl_checker.h"
#include "check/stmt_result.h"
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

  StmtResult CheckBlock(const ast::Block *, bool open_scope = true);
  StmtResult CheckStmt(const ast::AstNode *);
  StmtResult CheckRet(const ast::RetStmt *);
  StmtResult CheckVarDecl(const ast::VarDeclStmt *);
  StmtResult CheckAssign(const ast::AssignStmt *);
  StmtResult CheckExpr(const ast::AstNode *);
  StmtResult CheckLit(const ast::Literal *);
  StmtResult CheckIdent(const ast::Ident *);
  StmtResult CheckBinary(const ast::Binary *);
  StmtResult CheckUnary(const ast::Unary *);
  StmtResult CheckCall(const ast::Call *);
  StmtResult CheckArray(const ast::Array *);
  StmtResult CheckIndex(const ast::Index *);
  StmtResult CheckIf(const ast::If *);
};

}  // namespace felis

#endif  // FELIS_CHECK_TYPE_CHECKER_H_
