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
  DeclChecker(bool is_32bit)
      : current_scope_(std::make_shared<Scope>(nullptr)), is_32bit(is_32bit) {
    InsertBuiltinTypes();
  };
  void CheckGlobalLevel(const std::unique_ptr<ast::File> &);

  void OpenScope();
  void CloseScope();
  bool CanDecl(const std::string &);
  std::shared_ptr<Decl> LookupVarDecl(const std::string &);
  std::shared_ptr<Decl> LookupFuncDecl(const std::string &);
  std::shared_ptr<Type> LookupType(const std::string &);

  void InsertDecl(std::string name, std::shared_ptr<Decl> decl) {
    current_scope_->InsertDecl(name, decl);
  }

 private:
  std::shared_ptr<Scope> current_scope_;
  bool is_32bit;

  void InsertBuiltinTypes();
  std::shared_ptr<Decl> LookupDecl(const std::string &);
  std::shared_ptr<Decl> MakeFnDecl(bool isExt,
                                   const std::unique_ptr<ast::FnProto> &);
};

}  // namespace felis

#endif  // FELIS_CHECK_DECL_CHECKER_H_
