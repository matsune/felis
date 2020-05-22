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
  std::shared_ptr<Ty> type;

  Decl(std::string name, std::shared_ptr<Ty> type, Kind kind)
      : name(name), type(type), kind(kind) {}

  std::shared_ptr<FuncType> AsFuncType() {
    assert(IsFunc());
    return std::dynamic_pointer_cast<FuncType>(type);
  }

  bool IsAssignable() {
    switch (kind) {
      case Kind::EXT:
      case Kind::FN:
      case Kind::LET:
        return false;
      default:
        return true;
    }
  }

  bool IsFunc() {
    switch (kind) {
      case Kind::EXT:
      case Kind::FN:
        return true;
      default:
        return false;
    }
  }
};

}  // namespace felis

#endif  // FELIS_CHECK_DECL_H_
