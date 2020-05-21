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
    ty_map[n.get()] = ty;
    return ty;
  }

  void FinalizeTypes() {
    NodeTyMap finalized_map;
    for (auto &it : ty_map) {
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
      finalized_map[it.first] = final_ty;
    }
    ty_map = finalized_map;
  }

  void Debug() {
    std::cout << "-------------" << std::endl;
    for (auto &it : ty_map) {
      std::cout << "Node: " << it.first << ", Type: " << ToString(*it.second)
                << std::endl;
    }
    std::cout << "--------------" << std::endl;
  }

 private:
  IdentDeclMap decl_map_;
  NodeTyMap ty_map;
};

}  // namespace felis

#endif  // FELIS_CHECK_NODE_MAP_H_
