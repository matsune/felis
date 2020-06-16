#ifndef FELIS_CHECK_DECL_H_
#define FELIS_CHECK_DECL_H_

#include <assert.h>

#include <map>
#include <memory>
#include <string>

#include "check/type.h"
#include "node/ast.h"

namespace felis {

struct Decl {
  enum Kind { EXT, FN, ARG, VAR, LET };

  const std::string name;
  const Decl::Kind kind;
  std::shared_ptr<Type> type;

  Decl(std::string name, std::shared_ptr<Type> type, Decl::Kind kind)
      : name(name), type(type), kind(kind) {}

  bool IsAssignable() {
    switch (kind) {
      case Decl::Kind::EXT:
      case Decl::Kind::FN:
      case Decl::Kind::LET:
        return false;
      default:
        return true;
    }
  }

  bool IsFunc() {
    switch (kind) {
      case Decl::Kind::EXT:
      case Decl::Kind::FN:
        return true;
      default:
        return false;
    }
  }

  bool IsVariable() {
    switch (kind) {
      case Decl::Kind::ARG:
      case Decl::Kind::VAR:
      case Decl::Kind::LET:
        return true;
      default:
        return false;
    }
  }
};

}  // namespace felis

#endif  // FELIS_CHECK_DECL_H_
