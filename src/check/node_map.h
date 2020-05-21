#ifndef FELIS_CHECK_NODE_MAP_H_
#define FELIS_CHECK_NODE_MAP_H_

#include <map>
#include <memory>

#include "check/decl.h"
#include "node/ast.h"
#include "string/string.h"

namespace felis {

class NodeMap {
 public:
  using IdentDeclMap = std::map<ast::Ident *, std::shared_ptr<Decl>>;
  using NodeTyMap = std::map<const ast::AstNode *, std::shared_ptr<Ty>>;

  std::shared_ptr<Decl> GetDecl(const std::unique_ptr<ast::Ident> &t) const {
    return decl_map_.at(t.get());
  }

  void SetDecl(const std::unique_ptr<ast::Ident> &t,
               std::shared_ptr<Decl> decl) {
    decl_map_[t.get()] = decl;
  }

  template <typename T>
  std::shared_ptr<Ty> RecordType(const std::unique_ptr<T> &n,
                                 const std::shared_ptr<Ty> ty) {
    ty_map_[n.get()] = ty;
    return ty;
  }

  template <typename T>
  std::shared_ptr<Typed> GetType(const std::unique_ptr<T> &n) {
    return std::dynamic_pointer_cast<Typed>(ty_map_.at(n.get()));
  }

  void FinalizeTypes() {
    FinalizeDeclMapTypes();
    FinalizeTyMapTypes();
  }

  void Debug() {
    std::cout << "------DeclMap-------" << std::endl;
    for (auto &it : decl_map_) {
      std::cout << "Node: " << it.first << ", " << ToString(*it.second)
                << std::endl;
    }
    std::cout << "------TyMap-------" << std::endl;
    for (auto &it : ty_map_) {
      std::cout << "Node: " << it.first << ", Type: " << ToString(*it.second)
                << std::endl;
    }
    std::cout << "--------------" << std::endl;
  }

 private:
  IdentDeclMap decl_map_;
  NodeTyMap ty_map_;

  void FinalizeDeclMapTypes() {
    for (auto &it : decl_map_) {
      auto final_ty = FinalTy(it.second->type);
      if (final_ty->IsUntyped()) {
        if (final_ty->IsUntypedInt()) {
          final_ty = kTypeI32;
        } else if (final_ty->IsUntypedFloat()) {
          final_ty = kTypeF32;
        } else {
          UNREACHABLE
        }
      }
      it.second->type = final_ty;
    }
  }

  void FinalizeTyMapTypes() {
    for (auto &it : ty_map_) {
      auto final_ty = FinalTy(it.second);
      if (final_ty->IsUntyped()) {
        if (final_ty->IsUntypedInt()) {
          final_ty = kTypeI32;
        } else if (final_ty->IsUntypedFloat()) {
          final_ty = kTypeF32;
        } else {
          UNREACHABLE
        }
      }
      it.second = final_ty;
    }
  }
};

}  // namespace felis

#endif  // FELIS_CHECK_NODE_MAP_H_
