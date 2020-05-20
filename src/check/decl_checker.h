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
  DeclChecker(std::map<ast::Ident *, std::shared_ptr<Decl>> &decl_map)
      : current_scope_(std::make_shared<Scope>(nullptr)), decl_map_(decl_map){};
  void SetupBuiltin();
  void Check(const std::unique_ptr<ast::File> &);

 private:
  std::shared_ptr<Scope> current_scope_;
  std::shared_ptr<FuncType> current_func_;
  std::map<ast::Ident *, std::shared_ptr<Decl>> &decl_map_;

  std::shared_ptr<Decl> GetDecl(std::unique_ptr<ast::Ident> &t) {
    return decl_map_.at(t.get());
  }

  void SetDecl(std::unique_ptr<ast::Ident> &t, std::shared_ptr<Decl> decl) {
    decl_map_[t.get()] = decl;
  }

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
  std::shared_ptr<Typed> LookupType(std::string name);

  void DebugScope();
};

}  // namespace felis

#endif  // FELIS_CHECK_DECL_CHECKER_H_
