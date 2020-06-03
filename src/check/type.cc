#include "check/type.h"

#include "macro.h"
#include "string/string.h"

namespace felis {

bool Type::operator==(const Type& other) const {
  if (TypeKind() != other.TypeKind()) {
    return false;
  }
  if (IsUntyped() || other.IsUntyped()) {
    // untyped cannot compare
    UNREACHABLE
  }
  auto t = dynamic_cast<const FixedType*>(this);
  auto& o = dynamic_cast<const FixedType&>(other);
  return *t == o;
}

bool FixedType::operator==(const FixedType& other) const {
  if (kind != other.kind) return false;

  if (IsFunc()) {
    auto t = dynamic_cast<const FuncType*>(this);
    auto& o = dynamic_cast<const FuncType&>(other);
    return *t == o;
  } else if (IsArray()) {
    auto t = dynamic_cast<const ArrayType*>(this);
    auto& o = dynamic_cast<const ArrayType&>(other);
    return *t == o;
  } else if (IsPtr()) {
    auto t = dynamic_cast<const PtrType*>(this);
    auto& o = dynamic_cast<const PtrType&>(other);
    return *t == o;
  } else {
    return true;
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

std::shared_ptr<FixedType> ArchInt(bool is_32bit) {
  return is_32bit ? kTypeI32 : kTypeI64;
}

std::shared_ptr<Type> Underlying(std::shared_ptr<Type> ty) {
  if (ty->IsFixed()) {
    return ty;
  } else if (ty->IsUntyped()) {
    auto untyped = std::dynamic_pointer_cast<Untyped>(ty);
    if (untyped->Ref()) {
      return Underlying(untyped->Ref());
    } else {
      return ty;
    }
  }
  UNREACHABLE
}

std::shared_ptr<Type> FinalType(std::shared_ptr<Type> ty, bool is_32bit) {
  auto underlying_ty = Underlying(ty);
  if (underlying_ty->IsFixed()) {
    if (underlying_ty->IsArray()) {
      auto array_ty = std::dynamic_pointer_cast<ArrayType>(underlying_ty);
      auto elem = FinalType(array_ty->elem, is_32bit);
      return std::make_shared<ArrayType>(elem, array_ty->size);
    }
    return underlying_ty;
  } else if (underlying_ty->IsUntyped()) {
    auto untyped = std::dynamic_pointer_cast<Untyped>(underlying_ty);
    if (untyped->IsUntypedInt()) {
      return is_32bit ? kTypeI32 : kTypeI64;
    } else if (untyped->IsUntypedFloat()) {
      return kTypeF32;
    }
  }
  std::cout << "[unreachabel] underlying " << ToString(underlying_ty)
            << std::endl;
  UNREACHABLE
}

}  // namespace felis
