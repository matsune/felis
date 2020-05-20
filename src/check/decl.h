#ifndef FELIS_CHECK_DECL_H_
#define FELIS_CHECK_DECL_H_

#include <assert.h>

#include <memory>
#include <string>

#include "check/type.h"

namespace felis {

struct Decl {
  enum Kind { EXT, FN, ARG, VAR, LET };

  const std::string name;
  std::shared_ptr<Ty> type;
  const Decl::Kind kind;

  Decl(std::string name, std::shared_ptr<Ty> type, Kind kind)
      : name(name), type(type), kind(kind) {
    /* assert(type != nullptr); */
    /* assert(!type->IsVoid()); */
  }

  FuncType* AsFuncType() {
    assert(IsFunc());
    return (FuncType*)type.get();
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

  void Debug();
};

}  // namespace felis

#endif  // FELIS_CHECK_DECL_H_
