#ifndef FELIS_CHECK_CHECK_H_
#define FELIS_CHECK_CHECK_H_

#include <memory>
#include <string>
#include <vector>

#include "check/decl.h"
#include "check/hir.h"
#include "check/scope.h"
#include "check/type.h"
#include "string/string.h"
#include "syntax/ast.h"

namespace felis {

class Checker {
 public:
  Checker() : currentScope_(std::make_shared<Scope>(nullptr)){};
  void SetupBuiltin();
  std::unique_ptr<hir::File> Check(std::unique_ptr<ast::File>);

 private:
  std::shared_ptr<Scope> currentScope_;
  std::shared_ptr<Decl> currentFunc_;

  std::unique_ptr<hir::Block> CheckFnDecl(std::unique_ptr<ast::FnDecl>,
                                          std::unique_ptr<hir::FnDecl> &);

  std::unique_ptr<hir::Stmt> CheckStmt(std::unique_ptr<ast::Stmt>);
  std::unique_ptr<hir::RetStmt> CheckRetStmt(std::unique_ptr<ast::RetStmt>);
  std::unique_ptr<hir::VarDeclStmt> CheckVarDeclStmt(
      std::unique_ptr<ast::VarDeclStmt>);
  std::unique_ptr<hir::AssignStmt> CheckAssignStmt(
      std::unique_ptr<ast::AssignStmt>);
  std::unique_ptr<hir::IfStmt> CheckIfStmt(std::unique_ptr<ast::IfStmt>);
  std::unique_ptr<hir::Block> CheckBlock(std::unique_ptr<ast::Block>);

  std::unique_ptr<hir::Expr> MakeExpr(std::unique_ptr<ast::Expr> expr);
  std::unique_ptr<hir::Constant> MakeLit(std::unique_ptr<ast::Lit> lit);

  std::unique_ptr<hir::Constant> MakeConstBinary(std::unique_ptr<hir::Constant>,
                                                 std::unique_ptr<hir::Constant>,
                                                 ast::BinOp);
  std::unique_ptr<hir::Constant> MakeConstUnary(std::unique_ptr<hir::Constant>,
                                                ast::UnOp);

  std::unique_ptr<hir::IntConstant> ParseInt(std::unique_ptr<ast::Lit>);
  double ParseFloat(std::unique_ptr<ast::Lit>);
  std::unique_ptr<hir::Expr> TryExprTy(std::unique_ptr<hir::Expr>,
                                       std::shared_ptr<Type>);
  std::unique_ptr<hir::Expr> TryConstantTy(std::unique_ptr<hir::Constant>,
                                           std::shared_ptr<Type>);
  std::unique_ptr<hir::Binary> CheckBinary(std::unique_ptr<hir::Binary>);

  std::shared_ptr<Decl> InsertFnDecl(
      bool isExt, const std::unique_ptr<ast::FnProto> &proto);
  void OpenScope();
  void CloseScope();
  bool CanDecl(std::string name);
  std::shared_ptr<Decl> LookupDecl(std::string name);
  std::shared_ptr<Type> LookupType(std::string name);

  void DebugScope();
};

}  // namespace felis

#endif  // FELIS_CHECK_CHECK_H_
