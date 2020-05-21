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

  void Debug() {
    std::cout << "-------------" << std::endl;
    for (auto &it : ty_map) {
      auto final_ty = felis::FinalTy(it.second);
      std::cout << "Node: " << it.first << ", Type: " << ToString(*final_ty)
                << " use count: " << it.second.use_count() << std::endl;
    }
    std::cout << "--------------" << std::endl;
  }

 private:
  IdentDeclMap decl_map_;
  NodeTyMap ty_map;
};

}  // namespace felis

#endif  // FELIS_CHECK_NODE_MAP_H_
