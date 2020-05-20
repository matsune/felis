#ifndef FELIS_CHECK_CONSTANT_H_
#define FELIS_CHECK_CONSTANT_H_

#include "error/error.h"
#include "node/hir.h"

namespace felis {

//// CharConstant
///* std::unique_ptr<hir::Expr> MatchType(std::unique_ptr<hir::CharConstant> e,
///*/
///*                                      std::shared_ptr<Type> ty) { */
///*   auto begin = e->Begin(); */
///*   auto end = e->End(); */
///*   // char may be int or float */
///*   switch (ty->TypeKind()) { */
///*     case Type::CHAR: */
///*       return std::move(e); */
///*     case Type::I32: */
///*     case Type::I64: */
///*       return std::make_unique<hir::IntConstant>(begin, end, e->val); */
///*     case Type::F32: */
///*     case Type::F64: */
///*       return std::make_unique<hir::FloatConstant>(begin, end, e->val, ty);
///*/
///*     default: */
///*       throw LocError::Create(begin, "cannot cast char literal"); */
///*   } */
///* } */
//
//// IntConstant
// std::unique_ptr<hir::Expr> MatchType(std::unique_ptr<hir::IntConstant> e,
//                                     std::shared_ptr<Type> ty) {
//  auto begin = e->Begin();
//  auto end = e->End();
//  switch (ty->TypeKind()) {
//      /* case Type::CHAR: { */
//      /*   UNREACHABLE */
//      /*   /1* if (e->IsI32Size()) { *1/ */
//      /*   /1*   if (!is_valid_code_point(e->val)) *1/ */
//      /*   /1*     throw LocError::Create(e->Begin(), "invalid code point");
//      *1/
//       */
//      /*   /1*   return std::make_unique<hir::CharConstant>(begin, end,
//      e->val);
//       * *1/ */
//      /*   /1* } *1/ */
//      /*   throw LocError::Create(begin, "overflow char"); */
//      /* } break; */
//
//    case Type::I32: {
//      if (e->IsI32Size()) {
//        e->type = ty;
//        return std::move(e);
//      }
//      throw LocError::Create(begin, "overflow i32");
//    } break;
//
//    case Type::I64: {
//      e->type = ty;
//      return std::move(e);
//    } break;
//
//    case Type::F32: {
//      if (e->IsI32Size()) {
//        return std::make_unique<hir::FloatConstant>(begin, end, e->val);
//      }
//      throw LocError::Create(begin, "overflow f32");
//    } break;
//
//    case Type::F64: {
//      return std::make_unique<hir::FloatConstant>(begin, end, e->val);
//    } break;
//
//    default:
//      throw LocError::Create(begin, "can't cast");
//  }
//}
//
//// FloatConstant
// std::unique_ptr<hir::Expr> MatchType(std::unique_ptr<hir::FloatConstant> e,
//                                     std::shared_ptr<Type> ty) {
//  auto begin = e->Begin();
//  switch (ty->TypeKind()) {
//    case Type::F32: {
//      if (e->Ty()->IsF32()) {
//        e->type = ty;
//        return std::move(e);
//      }
//      throw LocError::Create(begin, "overflow f32");
//    } break;
//
//    case Type::F64: {
//      e->type = ty;
//      return std::move(e);
//    } break;
//
//    default:
//      throw LocError::Create(begin, "can't cast");
//  }
//}
//
//// BoolConstant
// std::unique_ptr<hir::Expr> MatchType(std::unique_ptr<hir::BoolConstant> e,
//                                     std::shared_ptr<Type> ty) {
//  if (ty->IsBool()) {
//    return std::move(e);
//  }
//  throw LocError::Create(e->Begin(), "can't cast bool");
//}
//
//// StringConstant
// std::unique_ptr<hir::Expr> MatchType(std::unique_ptr<hir::StringConstant> e,
//                                     std::shared_ptr<Type> ty) {
//  if (ty->IsString()) {
//    return std::move(e);
//  }
//  throw LocError::Create(e->Begin(), "can't cast string");
//}

}  // namespace felis

#endif  // FELIS_CHECK_CONSTANT_H_
