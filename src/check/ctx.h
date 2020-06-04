#ifndef FELIS_CHECK_CTX_H_
#define FELIS_CHECK_CTX_H_

#include <map>
#include <memory>

#include "check/decl_checker.h"
#include "check/stmt_result.h"
#include "check/ty_resolver.h"
#include "node/ast.h"

namespace felis {

class TypeCheckCtx {
 public:
  using IdentDeclMap = std::map<const ast::Ident *, std::shared_ptr<Decl>>;
  using ResultMap = std::map<const ast::Stmt *, StmtResult<>>;

  TypeCheckCtx(bool is_32bit) : is_32bit(is_32bit) {}

  template <typename T>
  void RecordDecl(const std::unique_ptr<T> &n, std::shared_ptr<Decl> ty) {
    ident_decl_map_[n.get()] = ty;
  }

  template <typename T>
  StmtResult<> RecordResult(const std::unique_ptr<T> &n, StmtResult<> result) {
    result_map_[n.get()] = result;
    return result;
  }

  const std::shared_ptr<Decl> &GetDecl(
      const std::unique_ptr<ast::Ident> &t) const {
    std::cout << "GetDecl " << t.get() << std::endl;
    return ident_decl_map_.at(t.get());
  }

  template <typename K>
  StmtResult<> GetResult(const std::unique_ptr<K> &n) const {
    return result_map_.at(n.get());
  }

  bool Is32bit() const { return is_32bit; }

  bool TryResolve(std::shared_ptr<Ty> ty, std::shared_ptr<Ty> to) {
    return resolver_.TryResolve(ty, to);
  }

  std::shared_ptr<Ty> ResolvedType(std::shared_ptr<Ty> ty) {
    return resolver_.ResolvedType(ty, is_32bit);
  }

  void FinalizeType() {
    // finalize types
    std::cout << "---------------" << std::endl;
    for (auto &it : ident_decl_map_) {
      it.second->type = ResolvedType(it.second->type);
      std::cout << "ident: " << it.first << " decl: " << ToString(it.second)
                << std::endl;
    }
    std::cout << "---------------" << std::endl;
    for (auto &it : result_map_) {
      if (it.second.IsExpr()) {
        it.second.val = ResolvedType(it.second.val);
        std::cout << "expr: " << it.first
                  << " type: " << ToString(it.second.val) << std::endl;
      }
    }
    std::cout << "---------------" << std::endl;
  }

 private:
  IdentDeclMap ident_decl_map_;
  ResultMap result_map_;
  TyResolver resolver_;
  const bool is_32bit;
};

}  // namespace felis

#endif  // FELIS_CHECK_CTX_H_
