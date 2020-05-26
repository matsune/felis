#include "check/type.h"

#include "macro.h"

namespace felis {

bool Type::operator==(const Type& other) const {
  if (TypeKind() != other.TypeKind()) {
    return false;
  }
  if (IsUntyped()) {
    UNREACHABLE
  }
  auto t = dynamic_cast<const FixedType*>(this);
  auto& o = dynamic_cast<const FixedType&>(other);
  return *t == o;
}

bool FixedType::operator==(const FixedType& other) const {
  if (FixedKind() != other.FixedKind()) {
    return false;
  }
  if (IsPrim()) {
    auto t = dynamic_cast<const PrimType*>(this);
    auto& o = dynamic_cast<const PrimType&>(other);
    return *t == o;
  } else if (IsFunc()) {
    auto t = dynamic_cast<const FuncType*>(this);
    auto& o = dynamic_cast<const FuncType&>(other);
    return *t == o;
  } else if (IsArray()) {
    auto t = dynamic_cast<const ArrayType*>(this);
    auto& o = dynamic_cast<const ArrayType&>(other);
    return *t == o;
  } else {
    UNREACHABLE
  }
}

std::shared_ptr<Untyped> Unresolved() {
  return std::make_shared<Untyped>(Untyped::Kind::UNRESOLVED);
}

std::shared_ptr<Untyped> UntypedInt() {
  return std::make_shared<Untyped>(Untyped::Kind::INT);
}

std::shared_ptr<Untyped> UntypedFloat() {
  return std::make_shared<Untyped>(Untyped::Kind::FLOAT);
}

std::shared_ptr<PrimType> ArchInt(bool is_32bit) {
  return is_32bit ? kTypeI32 : kTypeI64;
}

std::shared_ptr<Type> Underlying(std::shared_ptr<Type> ty) {
  if (ty->IsFixed()) {
    return ty;
  } else if (ty->IsUntyped()) {
    auto untyped = std::dynamic_pointer_cast<Untyped>(ty);
    if (untyped->Ref())
      return Underlying(untyped->Ref());
    else
      return ty;
  }
  UNREACHABLE
}

}  // namespace felis
