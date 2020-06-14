#include "type.h"

namespace felis {

bool Type::operator==(const Type &other) {
  // unresolved types cannot compare
  assert(!IsUntype());

  if (IsFunc()) {
    if (!other.IsFunc()) return false;

    auto args = GetArgs();
    for (int i = 0; i < args.size(); ++i) {
      if (*args.at(i) != *other.GetArgs().at(i)) return false;
    }
    return *GetRet() == *other.GetRet();
  }
  if (IsPtr()) {
    if (!other.IsPtr()) return false;
    return *GetElem() == *other.GetElem();
  }
  if (IsArray()) {
    if (!other.IsArray()) return false;
    return GetSize() == other.GetSize() && *GetElem() == *other.GetElem();
  }
  return GetKind() == other.GetKind();
}

bool Type::Substitutable(std::shared_ptr<Type> type) {
  if (IsUntype()) {
    if (ref_) return ref_->Substitutable(type);

    if (IsUnresolved()) return true;
    if (IsUntypedInt())
      return type->IsUntypedInt() || type->IsUntypedFloat() || type->IsI8() ||
             type->IsI16() || type->IsI32() || type->IsI64() || type->IsF32() ||
             type->IsF64();
    if (IsUntypedFloat())
      return type->IsUntypedFloat() || type->IsF32() || type->IsF64();
  }

  if (IsArray()) {
    return type->IsArray() && GetSize() == type->GetSize() &&
           GetElem()->Substitutable(type->GetElem());
  }

  if (IsFunc()) {
    if (!type->IsFunc()) return false;
    auto args = GetArgs();
    for (auto i = 0; i < args.size(); ++i) {
      if (args.at(i)->Substitutable(type->GetArgs().at(i))) return false;
    }
    return GetRet()->Substitutable(type->GetRet());
  }

  if (IsPtr()) {
    return type->IsPtr() && GetElem()->Substitutable(type->GetRet());
  }

  return GetKind() == type->GetKind();
}

void Type::Constraint(std::shared_ptr<Type> type) {
  assert(Substitutable(type));

  // check circular reference
  auto ref_ty = type;
  while (ref_ty) {
    if (ref_ty.get() == this) return;
    ref_ty = ref_ty->IsUntype() ? ref_ty->GetRef() : nullptr;
  }

  if (IsUntype()) {
    if (ref_) {
      ref_->Constraint(type);
    } else {
      ref_ = type;
    }
    return;
  }
  if (IsFunc()) {
    auto args = GetArgs();
    for (auto i = 0; i < args.size(); ++i) {
      args.at(i)->Constraint(type->GetArgs().at(i));
    }
    GetRet()->Constraint(type->GetRet());
    return;
  }
  if (IsArray()) {
    GetElem()->Constraint(type->GetElem());
    return;
  }
  if (IsPtr()) {
    GetElem()->Constraint(type->GetElem());
    return;
  }
}

void Type::Resolve(bool is_32bit) {
  if (IsUntype()) {
    Type *underlying = this;
    while (underlying && underlying->IsUntype() && underlying->GetRef()) {
      underlying = underlying->GetRef().get();
    }
    if (underlying->IsUntypedInt()) {
      underlying = is_32bit ? Type::MakeI32().get() : Type::MakeI64().get();
    }
    *this = *underlying;
    return;
  }
  if (IsFunc()) {
    auto args = GetArgs();
    for (auto i = 0; i < args.size(); ++i) {
      args.at(i)->Resolve(is_32bit);
    }
    GetRet()->Resolve(is_32bit);
    return;
  }
  if (IsArray() || IsPtr()) {
    GetElem()->Resolve(is_32bit);
    return;
  }
}

}  // namespace felis
