#ifndef FELIS_CHECK_DECL_CHECKER_H_
#define FELIS_CHECK_DECL_CHECKER_H_

#include <map>
#include <memory>
#include <string>
#include <vector>

#include "check/decl.h"
#include "check/scope.h"
#include "check/type.h"
#include "node/ast.h"
#include "string/string.h"

namespace felis {

class DeclChecker {
 public:
  DeclChecker(std::map<ast::AstNode *, std::shared_ptr<Decl>> &ast_decl)
      : current_scope_(std::make_shared<Scope>(nullptr)), ast_decl_(ast_decl){};
  void SetupBuiltin();
  void Check(const std::unique_ptr<ast::File> &);

 private:
  std::shared_ptr<Scope> current_scope_;
  FuncType *current_func_;
  std::map<ast::AstNode *, std::shared_ptr<Decl>> &ast_decl_;

  void CheckFnDecl(const std::unique_ptr<ast::FnDecl> &);
  void CheckStmt(const std::unique_ptr<ast::Stmt> &);
  void CheckExpr(const std::unique_ptr<ast::Expr> &);
  void CheckRet(const std::unique_ptr<ast::RetStmt> &);
  void CheckVarDecl(const std::unique_ptr<ast::VarDeclStmt> &);
  void CheckAssign(const std::unique_ptr<ast::AssignStmt> &);
  void CheckIf(const std::unique_ptr<ast::If> &);
  void CheckBlock(const std::unique_ptr<ast::Block> &);

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

#endif  // FELIS_CHECK_DECL_CHECKER_H_
