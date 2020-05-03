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

  std::vector<std::unique_ptr<hir::Stmt>> CheckFnDecl(
      std::unique_ptr<ast::FnDecl>&);

  std::unique_ptr<hir::Stmt> CheckStmt(std::unique_ptr<ast::Stmt>&);
  std::unique_ptr<hir::RetStmt> CheckRetStmt(ast::RetStmt*);
  std::unique_ptr<hir::VarDeclStmt> CheckVarDeclStmt(ast::VarDeclStmt*);
  std::unique_ptr<hir::AssignStmt> CheckAssignStmt(ast::AssignStmt*);
  std::unique_ptr<hir::IfStmt> CheckIfStmt(ast::IfStmt*);
  std::unique_ptr<hir::Block> CheckBlock(ast::Block*);

  std::unique_ptr<hir::Expr> MakeExpr(ast::Expr* expr);
  std::unique_ptr<hir::Constant> MakeLit(ast::Lit* lit);

  std::unique_ptr<hir::Constant> MakeConstBinary(hir::Constant*, hir::Constant*,
                                                 ast::BinOp);
  void MakeConstUnary(hir::Constant*, ast::UnOp);

  std::unique_ptr<hir::IntConstant> ParseInt(ast::Lit*);
  double ParseFloat(ast::Lit*);
  void TryExpTy(hir::Expr*, std::shared_ptr<Type>);
  void TryConstantTy(hir::Constant* cons, std::shared_ptr<Type> ty);
  void CheckBinary(std::unique_ptr<hir::Expr>& lhs,
                   std::unique_ptr<hir::Expr>& rhs, ast::BinOp op);

  std::shared_ptr<Decl> InsertFnDecl(
      bool isExt, const std::unique_ptr<ast::FnProto>& proto);
  void OpenScope();
  void CloseScope();
  bool CanDecl(std::string name);
  std::shared_ptr<Decl> LookupDecl(std::string name);
  std::shared_ptr<Type> LookupType(std::string name);

  void DebugScope();
};

}  // namespace felis

#endif  // FELIS_CHECK_CHECK_H_
