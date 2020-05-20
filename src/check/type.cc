#include "check/type.h"

namespace felis {

bool Ty::operator==(const Ty& other) {
  if (this->IsFunc() && other.IsFunc()) {
    auto l = (FuncType*)this;
    auto r = (FuncType&)other;
    if (l->args.size() != r.args.size()) return false;
    bool res = false;
    for (auto i = 0; i < l->args.size(); i++) {
      if (*l->args.at(i) != *r.args.at(i)) return false;
    }
    return *l->ret == *l->ret;
  } else if (this->IsTyped() && other.IsTyped()) {
    auto l = (Typed*)this;
    auto r = (Typed&)other;
    return l->TypedKind() == r.TypedKind();
  } else if (this->IsUntyped() && other.IsUntyped()) {
  }
  return false;
}

std::shared_ptr<Untyped> UntypedInt() {
  return std::make_shared<Untyped>(Untyped::Kind::INT);
}

std::shared_ptr<Untyped> UntypedFloat() {
  return std::make_shared<Untyped>(Untyped::Kind::FLOAT);
}

std::shared_ptr<Ty> FinalTy(std::shared_ptr<Ty>& ty) {
  if (ty->IsTyped()) return ty;
  auto untyped = (std::shared_ptr<felis::Untyped>&)ty;
  if (untyped->GetRef())
    return FinalTy(untyped->GetRef());
  else
    return ty;
}

}  // namespace felis
