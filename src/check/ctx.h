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
  using ResultMap = std::map<const ast::AstNode *, StmtResult>;

  TypeCheckCtx(bool is_32bit) : is_32bit(is_32bit) {}

  void RecordDecl(const ast::Ident *n, std::shared_ptr<Decl> ty) {
    ident_decl_map_[n] = ty;
  }

  StmtResult RecordResult(const ast::AstNode *n, StmtResult result) {
    result_map_[n] = result;
    return result;
  }

  const std::shared_ptr<Decl> &GetDecl(const ast::Ident *t) const {
    return ident_decl_map_.at(t);
  }

  StmtResult GetResult(const ast::AstNode *n) const {
    return result_map_.at(n);
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
    for (auto &it : ident_decl_map_) {
      it.second->type = ResolvedType(it.second->type);
    }
    for (auto &it : result_map_) {
      if (it.second.IsExpr()) {
        it.second.type = ResolvedType(it.second.type);
      }
    }
  }

 private:
  IdentDeclMap ident_decl_map_;
  ResultMap result_map_;
  TyResolver resolver_;
  const bool is_32bit;
};

}  // namespace felis

#endif  // FELIS_CHECK_CTX_H_
