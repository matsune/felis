//#include "check/lower.h"
//
//#include <assert.h>
//
//#include "check/parse.h"
//#include "error/error.h"
//#include "unique.h"
//
// namespace felis {
//
// namespace {
//
// hir::Unary::Op un_op_ast_to_hir(ast::UnaryOp::Op op) {
//  switch (op) {
//    case ast::UnaryOp::Op::NEG:
//      return hir::Unary::Op::NEG;
//    case ast::UnaryOp::Op::NOT:
//      return hir::Unary::Op::NOT;
//  }
//}
//
// hir::Binary::Op bin_op_ast_to_hir(ast::BinaryOp::Op op) {
//  switch (op) {
//    case ast::BinaryOp::Op::EQEQ:
//      return hir::Binary::Op::EQEQ;
//    case ast::BinaryOp::Op::NEQ:
//      return hir::Binary::Op::NEQ;
//    case ast::BinaryOp::Op::LT:
//      return hir::Binary::Op::LT;
//    case ast::BinaryOp::Op::LE:
//      return hir::Binary::Op::LE;
//    case ast::BinaryOp::Op::GT:
//      return hir::Binary::Op::GT;
//    case ast::BinaryOp::Op::GE:
//      return hir::Binary::Op::GE;
//    case ast::BinaryOp::Op::ADD:
//      return hir::Binary::Op::ADD;
//    case ast::BinaryOp::Op::SUB:
//      return hir::Binary::Op::SUB;
//    case ast::BinaryOp::Op::MUL:
//      return hir::Binary::Op::MUL;
//    case ast::BinaryOp::Op::DIV:
//      return hir::Binary::Op::DIV;
//    case ast::BinaryOp::Op::MOD:
//      return hir::Binary::Op::MOD;
//  }
//}
//
// std::shared_ptr<Type> FinalType(std::shared_ptr<Type> ty, bool is_32bit) {
//  auto underlying_ty = Underlying(ty);
//  if (underlying_ty->IsFixed()) {
//    if (underlying_ty->IsArray()) {
//      auto array_ty = std::dynamic_pointer_cast<ArrayType>(underlying_ty);
//      auto elem = FinalType(array_ty->elem, is_32bit);
//      return std::make_shared<ArrayType>(elem, array_ty->size);
//    }
//    return underlying_ty;
//  } else if (underlying_ty->IsUntyped()) {
//    auto untyped = std::dynamic_pointer_cast<Untyped>(underlying_ty);
//    if (untyped->IsUntypedInt()) {
//      return is_32bit ? kTypeI32 : kTypeI64;
//    } else if (untyped->IsUntypedFloat()) {
//      return kTypeF32;
//    }
//  }
//  std::cout << "[unreachabel] underlying " << ToString(underlying_ty)
//            << std::endl;
//  UNREACHABLE
//}
//
//}  // namespace
//
//// std::unique_ptr<hir::File> Lowering(std::unique_ptr<ast::File> file,
////                                    bool is_32bit) {
////  IdentDeclMap ident_decl_map;
////  ExprTypeMap expr_type_map;
////
////  TypeChecker type_ck(is_32bit, ident_decl_map, expr_type_map);
////  type_ck.Check(file);
////
////  /* IdentDeclMap final_ident_decl_map = ident_decl_map; */
////  /* ExprTypeMap final_expr_type_map = expr_type_map; */
////  // finalize types
////  std::cout << "---------------" << std::endl;
////  for (auto &it : ident_decl_map) {
////    std::cout << "ident: " << it.first << " decl: " << ToString(it.second)
////              << std::endl;
////    it.second->type = FinalType(it.second->type, is_32bit);
////    std::cout << " decl: " << ToString(it.second) << std::endl;
////  }
////  std::cout << "---------------" << std::endl;
////  for (auto &it : expr_type_map) {
////    std::cout << "expr: " << it.first;
////    it.second = FinalType(it.second, is_32bit);
////    std::cout << " type: " << ToString(it.second) << std::endl;
////  }
////  std::cout << "---------------" << std::endl;
////
////  //  std::cout << "---------------" << std::endl;
////  //  for (auto &it : final_ident_decl_map) {
////  //    final_ident_decl_map[it.first]->type = FinalType(it.second->type,
////  //    is_32bit); std::cout << "ident: " << it.first << " from: " <<
////  //    ToString(it.second)
////  //              << " to: " << ToString(it.second) << std::endl;
////  //  }
////  //  std::cout << "---------------" << std::endl;
////  //  for (auto &it : final_expr_type_map) {
////  //    std::cout << "expr: " << it.first << " type: " <<
///ToString(it.second) /  //              << std::endl; /  //  } /  // std::cout
///<< "---------------" << std::endl;
////
////  return nullptr;
////  /* return Lower(ident_decl_map, expr_type_map).Lowering(std::move(file));
///*/
////}
//
// std::unique_ptr<hir::File> Lower::Lowering(std::unique_ptr<ast::File> file) {
//  // Lowering AST to HIR
//  auto hir_file = std::make_unique<hir::File>();
//  while (!file->externs.empty()) {
//    auto ext = file->externs.move_front();
//    auto decl = GetDecl(ext->proto->name);
//    hir_file->externs.push_back(std::make_unique<hir::Extern>(decl));
//  }
//
//  while (!file->fn_decls.empty()) {
//    std::unique_ptr<ast::FnDecl> fn = file->fn_decls.move_front();
//    auto decl = GetDecl(fn->proto->name);
//
//    std::deque<std::shared_ptr<Decl>> args;
//    for (auto &arg : fn->proto->args->list) {
//      auto arg_decl = GetDecl(arg->name);
//      args.push_back(arg_decl);
//    }
//    auto block = LowerBlock(std::move(fn->block));
//
//    hir_file->fn_decls.push_back(
//        std::make_unique<hir::FnDecl>(decl, args, std::move(block)));
//  }
//  return std::move(hir_file);
//}
//
// std::unique_ptr<hir::Stmt> Lower::LowerStmt(std::unique_ptr<ast::Stmt> stmt)
// {
//  std::cout << "LowerStmt " << stmt->StmtKind() << std::endl;
//  switch (stmt->StmtKind()) {
//    case ast::Stmt::Kind::EXPR:
//      return LowerExpr(unique_cast<ast::Expr>(std::move(stmt)));
//    case ast::Stmt::Kind::RET:
//      return LowerRet(unique_cast<ast::RetStmt>(std::move(stmt)));
//    case ast::Stmt::Kind::VAR_DECL:
//      return LowerVarDecl(unique_cast<ast::VarDeclStmt>(std::move(stmt)));
//    case ast::Stmt::Kind::ASSIGN:
//      return LowerAssign(unique_cast<ast::AssignStmt>(std::move(stmt)));
//  }
//}
//
// std::unique_ptr<hir::RetStmt> Lower::LowerRet(
//    std::unique_ptr<ast::RetStmt> stmt) {
//  if (stmt->expr) {
//    return std::make_unique<hir::RetStmt>(LowerExpr(std::move(stmt->expr)));
//  } else {
//    return std::make_unique<hir::RetStmt>();
//  }
//}
//
// std::unique_ptr<hir::VarDeclStmt> Lower::LowerVarDecl(
//    std::unique_ptr<ast::VarDeclStmt> stmt) {
//  auto decl = GetDecl(stmt->name);
//  auto expr = LowerExpr(std::move(stmt->expr));
//  return std::make_unique<hir::VarDeclStmt>(decl, std::move(expr));
//}
//
// std::unique_ptr<hir::AssignStmt> Lower::LowerAssign(
//    std::unique_ptr<ast::AssignStmt> stmt) {
//  auto decl = GetDecl(stmt->name);
//  auto expr = LowerExpr(std::move(stmt->expr));
//  return std::make_unique<hir::AssignStmt>(decl, std::move(expr));
//}
//
// std::unique_ptr<hir::Expr> Lower::LowerExpr(std::unique_ptr<ast::Expr> expr)
// {
//  switch (expr->ExprKind()) {
//    case ast::Expr::Kind::LIT:
//      return LowerLit(unique_cast<ast::Lit>(std::move(expr)));
//
//    case ast::Expr::Kind::CALL: {
//      auto call = unique_cast<ast::CallExpr>(std::move(expr));
//      auto decl = GetDecl(call->ident);
//      auto func_decl = decl->AsFuncType();
//      unique_deque<hir::Expr> args;
//      while (!call->args.empty()) {
//        auto arg = call->args.move_front();
//        args.push_back(LowerExpr(std::move(arg)));
//      }
//      return std::make_unique<hir::Call>(decl, std::move(args));
//    } break;
//
//    case ast::Expr::Kind::IDENT: {
//      auto ident = unique_cast<ast::Ident>(std::move(expr));
//      auto decl = GetDecl(ident);
//      return std::make_unique<hir::Variable>(decl);
//    } break;
//
//    case ast::Expr::Kind::UNARY: {
//      auto unary = unique_cast<ast::UnaryExpr>(std::move(expr));
//      auto op = un_op_ast_to_hir(unary->op->op);
//      auto expr = LowerExpr(std::move(unary->expr));
//      return std::make_unique<hir::Unary>(op, std::move(expr));
//    } break;
//
//    case ast::Expr::Kind::BINARY: {
//      auto ty = GetType(expr);
//      auto binary = unique_cast<ast::BinaryExpr>(std::move(expr));
//      auto op = bin_op_ast_to_hir(binary->op->op);
//      auto lhs = LowerExpr(std::move(binary->lhs));
//      auto rhs = LowerExpr(std::move(binary->rhs));
//      return std::make_unique<hir::Binary>(ty, op, std::move(lhs),
//                                           std::move(rhs));
//    } break;
//    case ast::Expr::Kind::IF:
//      return LowerIf(unique_cast<ast::If>(std::move(expr)));
//    case ast::Expr::Kind::BLOCK:
//      return LowerBlock(unique_cast<ast::Block>(std::move(expr)));
//    case ast::Expr::Kind::ARRAY: {
//      auto array = unique_cast<ast::ArrayExpr>(std::move(expr));
//      auto ty = GetType(array);
//      unique_deque<hir::Expr> exprs;
//      while (!array->exprs.empty()) {
//        auto expr = array->exprs.move_front();
//        exprs.push_back(LowerExpr(std::move(expr)));
//      }
//      return std::make_unique<hir::Array>(ty, std::move(exprs));
//    } break;
//  }
//}
//
// std::unique_ptr<hir::If> Lower::LowerIf(std::unique_ptr<ast::If> expr) {
//  auto ty = GetType(expr);
//  auto cond = LowerExpr(std::move(expr->cond));
//  auto block = LowerBlock(std::move(expr->block));
//  std::unique_ptr<hir::Expr> els = nullptr;
//  if (expr->HasElse()) {
//    if (expr->IsElseIf()) {
//      els = LowerIf(unique_cast<ast::If>(std::move(expr->els)));
//    } else {
//      els = LowerBlock(unique_cast<ast::Block>(std::move(expr->els)));
//    }
//  }
//  return std::make_unique<hir::If>(ty, std::move(cond), std::move(block),
//                                   std::move(els));
//}
//
// std::unique_ptr<hir::Block> Lower::LowerBlock(
//    std::unique_ptr<ast::Block> block) {
//  auto ty = GetType(block);
//  unique_deque<hir::Stmt> stmts;
//  while (!block->stmts.empty()) {
//    stmts.push_back(LowerStmt(block->stmts.move_front()));
//  }
//  return std::make_unique<hir::Block>(ty, std::move(stmts));
//}
//
// std::unique_ptr<hir::Value> Lower::LowerLit(std::unique_ptr<ast::Lit> lit) {
//  switch (lit->LitKind()) {
//    case ast::Lit::Kind::CHAR: {
//      auto ty = GetType(lit);
//
//      std::stringstream ss(lit->val);
//      rune r;
//      ss >> r;
//
//      if (ty->IsI32() || ty->IsI64()) {
//        return std::make_unique<hir::IntConstant>(ty, r);
//      } else if (ty->IsF32() || ty->IsF64()) {
//        return std::make_unique<hir::FloatConstant>(ty, r);
//      } else {
//        UNREACHABLE
//      }
//
//    } break;
//    case ast::Lit::Kind::INT:
//      return ParseIntLit(std::move(lit));
//    case ast::Lit::Kind::FLOAT: {
//      return ParseFloatLit(std::move(lit));
//    } break;
//    case ast::Lit::Kind::BOOL: {
//      auto ty = GetType(lit);
//      return std::make_unique<hir::BoolConstant>(ty, lit->val == "true");
//    } break;
//    case ast::Lit::Kind::STRING: {
//      auto ty = GetType(lit);
//      return std::make_unique<hir::StringConstant>(ty, lit->val);
//    } break;
//  }
//}
//
// std::unique_ptr<hir::Value> Lower::ParseIntLit(std::unique_ptr<ast::Lit> lit)
// {
//  int64_t n;
//  std::string err;
//  if (!ParseInt(lit->val, n, err)) {
//    throw LocError::Create(lit->Begin(), err);
//  }
//
//  auto ty = GetType(lit);
//  if (ty->IsI8()) {
//    if (n < INT8_MIN || n > INT8_MAX) {
//      throw LocError::Create(lit->Begin(), "overflow int8");
//    }
//    return std::make_unique<hir::IntConstant>(ty, n);
//  } else if (ty->IsI16()) {
//    if (n < INT16_MIN || n > INT16_MAX) {
//      throw LocError::Create(lit->Begin(), "overflow int16");
//    }
//    return std::make_unique<hir::IntConstant>(ty, n);
//  } else if (ty->IsI32()) {
//    if (n < INT32_MIN || n > INT32_MAX) {
//      throw LocError::Create(lit->Begin(), "overflow int32");
//    }
//    return std::make_unique<hir::IntConstant>(ty, n);
//  } else if (ty->IsI64()) {
//    return std::make_unique<hir::IntConstant>(ty, n);
//  } else if (ty->IsF32() || ty->IsF64()) {
//    return std::make_unique<hir::FloatConstant>(ty, n);
//  } else {
//    UNREACHABLE
//  }
//}
//
// std::unique_ptr<hir::FloatConstant> Lower::ParseFloatLit(
//    std::unique_ptr<ast::Lit> lit) {
//  std::string err;
//  double n;
//  if (!ParseFloat(lit->val, n, err)) {
//    throw LocError::Create(lit->Begin(), err);
//  }
//  auto ty = GetType(lit);
//  assert(ty->IsFixedFloat());
//  return std::make_unique<hir::FloatConstant>(ty, n);
//}
//
//}  // namespace felis
