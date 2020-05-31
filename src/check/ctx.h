#ifndef FELIS_CHECK_CTX_H_
#define FELIS_CHECK_CTX_H_

#include <map>
#include <memory>

#include "check/decl_checker.h"
#include "node/ast.h"

namespace felis {

class TypeCheckCtx {
 public:
  using IdentDeclMap = std::map<const ast::Ident *, std::shared_ptr<Decl>>;
  using ExprTypeMap = std::map<const ast::Expr *, std::shared_ptr<Type>>;

  TypeCheckCtx(bool is_32bit) : is_32bit(is_32bit) {}

  template <typename T>
  void RecordDecl(const std::unique_ptr<T> &n, std::shared_ptr<Decl> ty) {
    ident_decl_map_[n.get()] = ty;
  }

  template <typename T>
  std::shared_ptr<Type> RecordType(const std::unique_ptr<T> &n,
                                   std::shared_ptr<Type> ty) {
    std::cout << "RecordType " << n.get() << " " << ToString(ty) << std::endl;
    expr_type_map_[n.get()] = ty;
    return ty;
  }

  auto &GetDecl(const std::unique_ptr<ast::Ident> &t) const {
    std::cout << "GetDecl " << t.get() << std::endl;
    return ident_decl_map_.at(t.get());
  }

  template <typename K>
  std::shared_ptr<FixedType> GetType(const std::unique_ptr<K> &n) const {
    return std::dynamic_pointer_cast<FixedType>(expr_type_map_.at(n.get()));
  }

  bool Is32bit() const { return is_32bit; }

  void FinalizeType() {
    // finalize types
    std::cout << "---------------" << std::endl;
    for (auto &it : ident_decl_map_) {
      it.second->type = FinalType(it.second->type, is_32bit);
      std::cout << "ident: " << it.first << " decl: " << ToString(it.second)
                << std::endl;
    }
    std::cout << "---------------" << std::endl;
    for (auto &it : expr_type_map_) {
      it.second = FinalType(it.second, is_32bit);
      std::cout << "expr: " << it.first << " type: " << ToString(it.second)
                << std::endl;
    }
    std::cout << "---------------" << std::endl;
  }

 private:
  IdentDeclMap ident_decl_map_;
  ExprTypeMap expr_type_map_;
  const bool is_32bit;
};

}  // namespace felis

#endif  // FELIS_CHECK_CTX_H_
