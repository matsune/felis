#include "ty.h"

namespace felis {

bool Ty::operator==(const Ty& other) const {
  if (kind != other.kind) return false;

  if (IsFunc()) {
    auto a = dynamic_cast<const FuncTy*>(this);
    auto b = dynamic_cast<const FuncTy&>(other);
    if (a->args.size() != b.args.size()) return false;
    for (auto i = 0; i < a->args.size(); ++i) {
      if (*a->args.at(i) != *b.args.at(i)) return false;
    }
    return *a->ret == *b.ret;
  }
  if (IsArray()) {
    auto a = dynamic_cast<const ArrayTy*>(this);
    auto b = dynamic_cast<const ArrayTy&>(other);
    if (a->size != b.size) return false;
    return *a->elem == *b.elem;
  }
  if (IsPtr()) {
    auto a = dynamic_cast<const PtrTy*>(this);
    auto b = dynamic_cast<const PtrTy&>(other);
    return *a->elem == *b.elem;
  }
  return true;
}

std::shared_ptr<Ty> ArchInt(bool is_32bit) {
  return is_32bit ? kTypeI32 : kTypeI64;
}

std::shared_ptr<Ty> UnResolved() {
  return std::make_shared<Ty>(Ty::Kind::UNRESOLVED);
}
std::shared_ptr<Ty> UntypedInt() {
  return std::make_shared<Ty>(Ty::Kind::UNTYPED_INT);
}
std::shared_ptr<Ty> UntypedFloat() {
  return std::make_shared<Ty>(Ty::Kind::UNTYPED_FLOAT);
}

}  // namespace felis
