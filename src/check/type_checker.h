#ifndef FELIS_CHECK_TYPE_CHECKER_H_
#define FELIS_CHECK_TYPE_CHECKER_H_

#include <map>
#include <memory>

#include "check/ctx.h"
#include "check/decl_checker.h"
#include "node/ast.h"

namespace felis {

class StmtResult {
 public:
  static StmtResult NonValue() {
    return StmtResult(Kind::NON_VALUE, kTypeVoid);
  }
  static StmtResult Ret() { return StmtResult(Kind::RET, kTypeVoid); }
  static StmtResult Expr(std::shared_ptr<Type> ty) {
    return StmtResult(Kind::EXPR, ty);
  }

  bool IsNonValue() const { return kind_ == StmtResult::Kind::NON_VALUE; }
  bool IsRet() const { return kind_ == StmtResult::Kind::RET; }
  bool IsExpr() const { return kind_ == StmtResult::Kind::EXPR; }

  std::shared_ptr<Type> GetType() const { return ty_; };

 private:
  // NON_VALUE: assign, var/let statement
  // RET: ret statement
  // EXPR: expr statements
  enum Kind { NON_VALUE, RET, EXPR };
  StmtResult::Kind kind_;
  std::shared_ptr<Type> ty_;

  StmtResult(StmtResult::Kind kind, std::shared_ptr<Type> ty)
      : kind_(kind), ty_(ty) {}
};

class TypeChecker {
 public:
  TypeChecker(TypeCheckCtx &ctx) : ctx_(ctx), decl_ck_(ctx.Is32bit()) {}

  void Check(const std::unique_ptr<ast::File> &);

 private:
  TypeCheckCtx &ctx_;
  DeclChecker decl_ck_;
  std::shared_ptr<FuncType> current_func_;

  StmtResult CheckBlock(const std::unique_ptr<ast::Block> &,
                        bool open_scope = true);
  StmtResult CheckStmt(const std::unique_ptr<ast::Stmt> &);
  StmtResult CheckRet(const std::unique_ptr<ast::RetStmt> &);
  StmtResult CheckVarDecl(const std::unique_ptr<ast::VarDeclStmt> &);
  StmtResult CheckAssign(const std::unique_ptr<ast::AssignStmt> &);
  StmtResult CheckExpr(const std::unique_ptr<ast::Expr> &);
  StmtResult CheckLit(const std::unique_ptr<ast::Lit> &);
  StmtResult CheckIdent(const std::unique_ptr<ast::Ident> &);
  StmtResult CheckBinary(const std::unique_ptr<ast::BinaryExpr> &);
  StmtResult CheckUnary(const std::unique_ptr<ast::UnaryExpr> &);
  StmtResult CheckCall(const std::unique_ptr<ast::CallExpr> &);
  StmtResult CheckArray(const std::unique_ptr<ast::ArrayExpr> &);
  StmtResult CheckIf(const std::unique_ptr<ast::If> &);
};

}  // namespace felis

#endif  // FELIS_CHECK_TYPE_CHECKER_H_
