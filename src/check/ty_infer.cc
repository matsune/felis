#include "check/ty_infer.h"

#include "error/error.h"

namespace felis {

bool type_canbe(const std::shared_ptr<Type> from,
                const std::shared_ptr<Type> to) {
  switch (from->TypeKind()) {
    case Type::Kind::UNRESOLVED:
      return true;
    case Type::Kind::UNTYPED_INT:
      return to->IsUntyped() || to->IsTypedInt() || to->IsTypedFloat();
    case Type::Kind::UNTYPED_FLOAT:
      return to->IsUntypedFloat() || to->IsTypedFloat();
    default:
      return *from == *to;
  }
}

void TyInfer::Infer(const std::unique_ptr<ast::File>& file) {
  for (auto& fn_decl : file->fn_decls) {
    auto decl = ast_decl_[fn_decl.get()]->AsFuncType();
    current_func_ = decl;
    auto _ = InferBlock(fn_decl->block);
  }
}

std::shared_ptr<Type> TyInfer::InferStmt(
    const std::unique_ptr<ast::Stmt>& stmt) {
  switch (stmt->StmtKind()) {
    case ast::Stmt::Kind::EXPR:
      return InferExpr((std::unique_ptr<ast::Expr>&)stmt);
    case ast::Stmt::Kind::RET:
      InferRet((std::unique_ptr<ast::RetStmt>&)stmt);
      break;
    case ast::Stmt::Kind::VAR_DECL:
      InferVarDecl((std::unique_ptr<ast::VarDeclStmt>&)stmt);
      break;
    case ast::Stmt::Kind::ASSIGN:
      InferAssign((std::unique_ptr<ast::AssignStmt>&)stmt);
      break;
  }
  return kTypeVoid;
}

void TyInfer::InferRet(const std::unique_ptr<ast::RetStmt>& stmt) {
  if (stmt->expr) {
    auto expr_ty = InferExpr(stmt->expr);
    if (!type_canbe(expr_ty, current_func_->ret)) {
      throw LocError::Create(stmt->Begin(), "mismatched ret type");
    }
    MakeType(stmt->expr.get(), current_func_->ret);
  }
}

void TyInfer::InferVarDecl(const std::unique_ptr<ast::VarDeclStmt>& stmt) {
  auto decl = ast_decl_[stmt.get()];
  auto expr_ty = InferExpr(stmt->expr);
  if (expr_ty->IsVoid()) {
    throw LocError::Create(stmt->Begin(), "cannot decl void type var");
  }
  *decl->type = *expr_ty;
}

void TyInfer::InferAssign(const std::unique_ptr<ast::AssignStmt>& stmt) {
  auto decl = ast_decl_.at(stmt.get());
  auto expr_ty = InferExpr(stmt->expr);
  if (type_canbe(expr_ty, decl->type)) {
    expr_ty = decl->type;
  } else if (type_canbe(decl->type, expr_ty)) {
    *decl->type = *expr_ty;
  } else {
    throw LocError::Create(stmt->expr->Begin(), "mismatched assign type");
  }
  MakeType(stmt->expr.get(), expr_ty);
}

std::shared_ptr<Type> TyInfer::InferExpr(
    const std::unique_ptr<ast::Expr>& expr) {
  switch (expr->ExprKind()) {
    case ast::Expr::Kind::LIT: {
      auto& lit = (std::unique_ptr<ast::Lit>&)expr;
      switch (lit->LitKind()) {
        case ast::Lit::Kind::BOOL:
          return kTypeBool;
        case ast::Lit::Kind::STRING:
          return kTypeString;
        case ast::Lit::Kind::CHAR:
        case ast::Lit::Kind::INT:
          return MakeUntypedInt();
        case ast::Lit::Kind::FLOAT:
          return MakeUntypedFloat();
      }
    } break;

    case ast::Expr::Kind::CALL: {
      auto& call_expr = (std::unique_ptr<ast::CallExpr>&)expr;
      auto decl = ast_decl_.at(call_expr.get())->AsFuncType();

      auto i = 0;
      for (auto& arg : call_expr->args) {
        auto arg_ty = InferExpr(arg);
        if (type_canbe(arg_ty, decl->args[i])) {
          arg_ty = decl->args[i];
        } else {
          throw LocError::Create(arg->Begin(), "mismatched arg ty");
        }
        MakeType(arg.get(), arg_ty);
        i++;
      }

      return decl->ret;

    } break;

    case ast::Expr::Kind::IDENT: {
      auto& ident = (std::unique_ptr<ast::Ident>&)expr;
      auto decl = ast_decl_.at(ident.get());
      return decl->type;
    } break;

    case ast::Expr::Kind::UNARY: {
      auto& unary_expr = (std::unique_ptr<ast::UnaryExpr>&)expr;
      auto expr_ty = InferExpr(unary_expr->expr);
      // same with expr
      return expr_ty;
    } break;

    case ast::Expr::Kind::BINARY: {
      auto& binary = (std::unique_ptr<ast::BinaryExpr>&)expr;
      auto lhs_ty = InferExpr(binary->lhs);
      auto rhs_ty = InferExpr(binary->rhs);
      std::shared_ptr<Type> operand_ty;

      if (type_canbe(lhs_ty, rhs_ty)) {
        lhs_ty = rhs_ty;
        operand_ty = rhs_ty;
      } else if (type_canbe(rhs_ty, lhs_ty)) {
        rhs_ty = lhs_ty;
        operand_ty = lhs_ty;
      } else {
        throw LocError::Create(binary->lhs->Begin(), "unmatch type");
      }

      assert(*operand_ty == *lhs_ty);
      assert(*operand_ty == *rhs_ty);
      assert(!operand_ty->IsUnresolved());
      MakeType(binary->lhs.get(), lhs_ty);
      MakeType(binary->rhs.get(), rhs_ty);

      std::shared_ptr<Type> final_ty;
      switch (binary->op->op) {
        case ast::BinaryOp::Op::EQEQ:
        case ast::BinaryOp::Op::NEQ:
        case ast::BinaryOp::Op::LT:
        case ast::BinaryOp::Op::LE:
        case ast::BinaryOp::Op::GT:
        case ast::BinaryOp::Op::GE:
          if (operand_ty->IsString() || operand_ty->IsVoid()) {
            throw LocError::Create(binary->Begin(),
                                   "cannot use this type for comparison");
          }
          final_ty = kTypeBool;
          break;

        case ast::BinaryOp::Op::ADD:
        case ast::BinaryOp::Op::SUB:
        case ast::BinaryOp::Op::MUL:
        case ast::BinaryOp::Op::DIV:
          if (!operand_ty->IsUntyped() && !operand_ty->IsTypedInt() &&
              !operand_ty->IsTypedFloat()) {
            throw LocError::Create(binary->Begin(),
                                   "cannot use non numeric type of binary");
          }
          final_ty = operand_ty;
          break;
        case ast::BinaryOp::Op::MOD:
          if (!operand_ty->IsUntyped() && !operand_ty->IsTypedInt()) {
            throw LocError::Create(binary->Begin(),
                                   "cannot use non numeric type of binary");
          }
          final_ty = operand_ty;
          break;
      }
      return final_ty;
    } break;
    case ast::Expr::Kind::IF:
      return InferIf((std::unique_ptr<ast::If>&)expr);
    case ast::Expr::Kind::BLOCK:
      return InferBlock((std::unique_ptr<ast::Block>&)expr);
  }
}  // namespace felis

std::shared_ptr<Type> TyInfer::InferIf(const std::unique_ptr<ast::If>& e) {
  auto cond_ty = InferExpr(e->cond);
  if (type_canbe(cond_ty, kTypeBool)) {
    cond_ty = kTypeBool;
  } else {
    throw LocError::Create(e->cond->Begin(), "not bool type");
  }
  MakeType(e->cond.get(), cond_ty);
  MakeType(e->block.get(), InferBlock(e->block));
  if (e->HasElse()) {
    if (e->IsElseIf()) {
      MakeType(e->els.get(), InferIf((std::unique_ptr<ast::If>&)e->els));
    } else {
      MakeType(e->els.get(), InferBlock((std::unique_ptr<ast::Block>&)e->els));
    }
  }
  return MakeUnresolved();
}

std::shared_ptr<Type> TyInfer::InferBlock(
    const std::unique_ptr<ast::Block>& e) {
  std::shared_ptr<Type> ty = kTypeVoid;
  for (auto& stmt : e->stmts) {
    auto stmt_ty = InferStmt(stmt);
    MakeType(stmt.get(), stmt_ty);
    ty = stmt_ty;
  }
  return ty;
}

}  // namespace felis

