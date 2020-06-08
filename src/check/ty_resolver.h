#ifndef FELIS_CHECK_TY_RESOLVER_H_
#define FELIS_CHECK_TY_RESOLVER_H_

#include <cassert>
#include <iostream>
#include <map>

#include "macro.h"
#include "string/string.h"
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

      if (auto func_ty = std::dynamic_pointer_cast<FuncTy>(ty)) {
        if (auto func_to = std::dynamic_pointer_cast<FuncTy>(to)) {
          for (auto i = 0; i < func_ty->args.size(); ++i) {
            TryResolve(func_ty->args.at(i), func_to->args.at(i));
          }
          TryResolve(func_ty->ret, func_to->ret);
        }
      } else if (auto array_ty = std::dynamic_pointer_cast<ArrayTy>(ty)) {
        if (auto array_to = std::dynamic_pointer_cast<ArrayTy>(to)) {
          TryResolve(array_ty->elem, array_to->elem);
        }
      } else if (auto ptr_ty = std::dynamic_pointer_cast<PtrTy>(ty)) {
        if (auto ptr_to = std::dynamic_pointer_cast<PtrTy>(to)) {
          TryResolve(ptr_ty->ref, ptr_to->ref);
        }
      }

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
        break;
    }
    if (auto func_ty = std::dynamic_pointer_cast<FuncTy>(ty)) {
      for (auto i = 0; i < func_ty->args.size(); ++i) {
        func_ty->args.at(i) = ResolvedType(func_ty->args.at(i), is_32bit);
      }
      func_ty->ret = ResolvedType(func_ty->ret, is_32bit);
      return func_ty;
    } else if (auto array_ty = std::dynamic_pointer_cast<ArrayTy>(ty)) {
      array_ty->elem = ResolvedType(array_ty->elem, is_32bit);
      return array_ty;
    } else if (auto ptr_ty = std::dynamic_pointer_cast<PtrTy>(ty)) {
      ptr_ty->ref = ResolvedType(ptr_ty->ref, is_32bit);
      return ptr_ty;
    }
    return ty;
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
      return IsFixed(ptr_ty->ref);
    } else {
      return !ty->IsUntyped();
    }
  }

  std::shared_ptr<Ty> Underlying(std::shared_ptr<Ty> ty) {
    assert(ty);
    while (true) {
      if (auto ref = ref_map[ty]) {
        ty = ref;
        continue;
      }
      if (auto func_ty = std::dynamic_pointer_cast<FuncTy>(ty)) {
        for (auto i = 0; i < func_ty->args.size(); ++i) {
          func_ty->args.at(i) = Underlying(func_ty->args.at(i));
        }
        func_ty->ret = Underlying(func_ty->ret);
        return func_ty;
      } else if (auto array_ty = std::dynamic_pointer_cast<ArrayTy>(ty)) {
        array_ty->elem = Underlying(array_ty->elem);
        return array_ty;
      } else if (auto ptr_ty = std::dynamic_pointer_cast<PtrTy>(ty)) {
        ptr_ty->ref = Underlying(ptr_ty->ref);
        return ptr_ty;
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
      return to->IsUntypedInt() || to->IsUntypedFloat() || to->IsNum();
    if (ty->IsUntypedFloat()) return to->IsUntypedFloat() || to->IsFloat();

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
        return Canbe(ptr_ty->ref, ptr_to->ref);
      }
    }

    return ty->kind == to->kind;
  }
};

}  // namespace felis

#endif  // FELIS_CHECK_TY_RESOLVER_H_
