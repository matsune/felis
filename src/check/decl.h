#ifndef FELIS_CHECK_DECL_H_
#define FELIS_CHECK_DECL_H_

#include <assert.h>

#include <map>
#include <memory>
#include <string>

#include "check/type.h"
#include "node/ast.h"

namespace felis {

enum DeclKind { EXT, FN, ARG, VAR, LET };

struct Decl {
  const std::string name;
  const DeclKind kind;
  std::shared_ptr<Type> type;

  Decl(std::string name, std::shared_ptr<Type> type, DeclKind kind)
      : name(std::move(name)), type(type), kind(kind) {}

  std::shared_ptr<PrimType> AsPrimType() {
    return std::dynamic_pointer_cast<PrimType>(type);
  }

  std::shared_ptr<FuncType> AsFuncType() {
    assert(IsFunc());
    return std::dynamic_pointer_cast<FuncType>(type);
  }

  bool IsAssignable() {
    switch (kind) {
      case DeclKind::EXT:
      case DeclKind::FN:
      case DeclKind::LET:
        return false;
      default:
        return true;
    }
  }

  bool IsFunc() {
    switch (kind) {
      case DeclKind::EXT:
      case DeclKind::FN:
        return true;
      default:
        return false;
    }
  }
};

}  // namespace felis

#endif  // FELIS_CHECK_DECL_H_
