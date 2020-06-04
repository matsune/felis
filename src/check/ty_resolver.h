#ifndef FELIS_CHECK_TY_RESOLVER_H_
#define FELIS_CHECK_TY_RESOLVER_H_

#include <cassert>
#include <iostream>
#include <map>

#include "macro.h"
#include "ty.h"

namespace felis {

class TyResolver {
 public:
  TyResolver() = default;

  bool TryResolve(std::shared_ptr<Ty> ty, std::shared_ptr<Ty> to) {
    ty = Underlying(ty);
    to = Underlying(to);
    if (to == ty) return true;
    if (Canbe(ty, to)) {
      assert(!ref_map[ty]);
      ref_map[ty] = to;
      return true;
    } else {
      return false;
    }
  }

  std::shared_ptr<Ty> ResolvedType(std::shared_ptr<Ty> ty, bool is_32bit) {
    ty = Underlying(ty);
    switch (ty->kind) {
      case Ty::Kind::UNRESOLVED:
        UNREACHABLE
        return nullptr;
      case Ty::Kind::UNTYPED_INT:
        return is_32bit ? kTypeI32 : kTypeI64;
      case Ty::Kind::UNTYPED_FLOAT:
        return kTypeF32;
      default:
        return ty;
    }
  }

 private:
  std::map<std::shared_ptr<Ty>, std::shared_ptr<Ty>> ref_map;

  bool IsFixed(std::shared_ptr<Ty> ty) {
    if (auto func_ty = std::dynamic_pointer_cast<FuncTy>(ty)) {
      for (auto arg : func_ty->args) {
        if (!IsFixed(arg)) return false;
      }
      return IsFixed(func_ty->ret);
    } else if (auto array_ty = std::dynamic_pointer_cast<ArrayTy>(ty)) {
      return IsFixed(array_ty->elem);
    } else if (auto ptr_ty = std::dynamic_pointer_cast<PtrTy>(ty)) {
      return IsFixed(ptr_ty->elem);
    } else {
      return !ty->IsUntyped();
    }
  }

  std::shared_ptr<Ty> Underlying(std::shared_ptr<Ty> ty) {
    assert(ty);
    while (true) {
      if (IsFixed(ty)) {
        return ty;
      }
      if (auto ref = ref_map[ty]) {
        ty = ref;
      } else {
        return ty;
      }
    }
  }

  bool Canbe(std::shared_ptr<Ty> ty, std::shared_ptr<Ty> to) {
    /* clang-format off */
    //
    // +---------------+------------+-------------+---------------+-----+-------+---+
    // |               | unresolved | untyped int | untyped float | int | float | * |
    // +---------------+------------+-------------+---------------+-----+-------+---+
    // |   unresolved  |      ○     |      ○      |       ○       |  ○  |   ○   | ○ |
    // +---------------+------------+-------------+---------------+-----+-------+---+
    // |  untyped int  |      x     |      ○      |       ○       |  ○  |   ○   | x |
    // +---------------+------------+-------------+---------------+-----+-------+---+
    // | untyped float |      x     |      x      |       ○       |  x  |   ○   | x |
    // +---------------+------------+-------------+---------------+-----+-------+---+
    //
    /* clang-format on */
    if (ty->IsUnResolved()) return true;
    if (ty->IsUntypedInt())
      return to->IsUntypedInt() || to->IsUntypedFloat() || to->IsInt() ||
             to->IsFloat();
    if (ty->IsUntypedFloat()) return to->IsUntypedFloat() || to->IsFloat();

    // [T, n] = [S, k]
    // n == k && T == S
    if (auto array_ty = std::dynamic_pointer_cast<ArrayTy>(ty)) {
      if (auto array_to = std::dynamic_pointer_cast<ArrayTy>(to)) {
        return array_ty->size == array_ty->size &&
               Canbe(array_ty->elem, array_to->elem);
      } else {
        return false;
      }
    }

    if (auto func_ty = std::dynamic_pointer_cast<FuncTy>(ty)) {
      if (auto func_to = std::dynamic_pointer_cast<FuncTy>(to)) {
        if (func_ty->args.size() != func_to->args.size()) return false;
        for (auto i = 0; i < func_ty->args.size(); ++i) {
          if (!Canbe(func_ty->args.at(i), func_to->args.at(i))) return false;
        }
        return Canbe(func_ty->ret, func_to->ret);
      } else {
        return false;
      }
    }

    if (auto ptr_ty = std::dynamic_pointer_cast<PtrTy>(ty)) {
      if (auto ptr_to = std::dynamic_pointer_cast<PtrTy>(to)) {
        return Canbe(ptr_ty->elem, ptr_to->elem);
      }
    }

    return ty->kind == to->kind;
  }
};

}  // namespace felis

#endif  // FELIS_CHECK_TY_RESOLVER_H_
