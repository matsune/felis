#ifndef FELIS_CHECK_TYPE_MAPS_H_
#define FELIS_CHECK_TYPE_MAPS_H_

#include <map>
#include <memory>

#include "check/decl_checker.h"
#include "check/eval.h"
#include "error/error.h"
#include "macro.h"
#include "node/ast.h"

namespace felis {

class TypeMaps {
 public:
  using IdentDeclMap = std::map<const ast::Ident *, std::shared_ptr<Decl>>;
  using ResultMap = std::map<const ast::AstNode *, Eval>;

  TypeMaps(bool is_32bit) : is_32bit(is_32bit) {}

  void RecordDecl(const ast::Ident *n, std::shared_ptr<Decl> ty) {
    ident_decl_map_[n] = ty;
  }

  Eval RecordResult(const ast::AstNode *n, Eval result) {
    result_map_[n] = result;
    return result;
  }

  const std::shared_ptr<Decl> &GetDecl(const ast::Ident *t) const {
    return ident_decl_map_.at(t);
  }

  Eval GetResult(const ast::AstNode *n) const { return result_map_.at(n); }

  bool Is32bit() const { return is_32bit; }

  bool TryResolve(std::shared_ptr<Type> type, std::shared_ptr<Type> to) {
    if (!type->Substitutable(to)) return false;
    type->Constraint(to);
    return true;
  }

  void ResolveTypes() {
    for (auto it : ident_decl_map_) {
      it.second->type->Resolve(is_32bit);
      if (!it.second->type->IsResolved()) {
        std::cout << ToString(*it.second->type) << " " << it.first << std::endl;
        throw LocError(it.first->begin, "cannot infer type");
      }
    }
    for (auto it : result_map_) {
      if (it.second.IsExpr()) {
        it.second.type->Resolve(is_32bit);
        if (!it.second.type->IsResolved()) {
          std::cout << ToString(*it.second.type) << " " << it.first
                    << std::endl;
          throw LocError(it.first->begin, "cannot infer type");
        }
      }
    }
  }

 private:
  IdentDeclMap ident_decl_map_;
  ResultMap result_map_;
  const bool is_32bit;
};

}  // namespace felis

#endif  // FELIS_CHECK_TYPE_MAPS_H_
