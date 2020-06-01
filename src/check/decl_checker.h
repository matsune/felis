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

  // Check and insert top level functions
  void CheckGlobalLevel(const std::unique_ptr<ast::File> &);

  void OpenScope();
  void CloseScope();

  // Check redeclaration
  bool ExistsInThisScope(const std::string &);

  std::shared_ptr<Decl> LookupVarDecl(const std::string &);
  std::shared_ptr<Decl> LookupFuncDecl(const std::string &);
  std::shared_ptr<Type> LookupType(const std::unique_ptr<ast::Type> &);

  void InsertDecl(std::string name, std::shared_ptr<Decl> decl) {
    current_scope_->InsertDecl(name, decl);
  }

 private:
  std::shared_ptr<Scope> current_scope_;
  bool is_32bit;

  // Insert builtin types into global scope.
  void InsertBuiltinTypes();

  std::shared_ptr<Decl> LookupDecl(const std::string &);

  // Checking below and create Decl if all passed.
  // - Function name uniqueness
  // - Arg type name validity
  // - Return type name validity
  std::shared_ptr<Decl> MakeFnDecl(bool isExt,
                                   const std::unique_ptr<ast::FnProto> &);
};

}  // namespace felis

#endif  // FELIS_CHECK_DECL_CHECKER_H_
