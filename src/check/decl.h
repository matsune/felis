#ifndef FELIS_CHECK_DECL_H_
#define FELIS_CHECK_DECL_H_

#include <assert.h>

#include <memory>
#include <string>

#include "check/type.h"

namespace felis {

struct Decl {
  enum Kind { EXT, FN, ARG, VAR, LET };
  std::string name;
  std::shared_ptr<Type> type;
  Kind kind;

  Decl(std::string name, std::shared_ptr<Type> type, Kind kind)
      : name(name), type(std::move(type)), kind(kind) {}

  FuncType* AsFuncType() {
    assert(IsFunc());
    return (FuncType*)type.get();
  }

  bool IsAssignable() {
    switch (kind) {
      case Kind::EXT:
      case Kind::FN:
      case Kind::VAR:
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

  void Debug();
};

}  // namespace felis

#endif  // FELIS_CHECK_DECL_H_
