#include "check/ty_infer.h"

#include "error/error.h"

namespace felis {

bool Resolve(std::shared_ptr<Ty> ty, std::shared_ptr<Ty> to) {
  if (ty->IsTyped()) {
    if (*ty == *to) {
      return true;
    }
  } else {
    auto untyped = (std::shared_ptr<Untyped>&)ty;
    if (untyped->Canbe(to)) {
      untyped->SetRef(to);
      return true;
    }
  }
  return false;
}

void TyInfer::Infer(std::unique_ptr<ast::File>& file) {
  for (auto& fn_decl : file->fn_decls) {
    auto decl = GetDecl(fn_decl->proto->name)->AsFuncType();
    current_func_ = decl;

    auto block_ty = InferBlock(fn_decl->block);

    if (!current_func_->IsVoid() && !fn_decl->block->IsTerminating()) {
      // last stmt can be ret stmt
      if (!Resolve(block_ty, current_func_->ret)) {
        throw LocError::Create(fn_decl->End(), "func block type not match");
      }
      assert(fn_decl->block->stmts.back()->StmtKind() == ast::Stmt::Kind::EXPR);
      auto last = unique_cast<ast::Expr>(fn_decl->block->stmts.move_back());
      auto n = std::make_unique<ast::RetStmt>(last->Begin(), last->End(),
                                              std::move(last));
      fn_decl->block->stmts.push_back(std::move(n));
    }
  }

  std::cout << "-------------" << std::endl;
  for (auto& it : ty_map) {
    auto final_ty = felis::FinalTy(it.second);
    std::cout << "Node: " << it.first << ", Type: " << ToString(*final_ty)
              << " use count: " << it.second.use_count() << std::endl;
  }
  std::cout << "--------------" << std::endl;
}

std::shared_ptr<Ty> TyInfer::InferStmt(const std::unique_ptr<ast::Stmt>& stmt) {
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
    if (!Resolve(expr_ty, current_func_->ret)) {
      throw LocError::Create(stmt->Begin(), "mismatched ret type");
    }
  }
}

void TyInfer::InferVarDecl(const std::unique_ptr<ast::VarDeclStmt>& stmt) {
  auto decl = GetDecl(stmt->name);
  auto expr_ty = InferExpr(stmt->expr);
  if (expr_ty->IsVoid()) {
    throw LocError::Create(stmt->Begin(), "cannot decl void type var");
  }
  assert(decl->type == nullptr);
  decl->type = expr_ty;
}

void TyInfer::InferAssign(const std::unique_ptr<ast::AssignStmt>& stmt) {
  auto decl = GetDecl(stmt->name);
  auto expr_ty = InferExpr(stmt->expr);
  if (!Resolve(expr_ty, decl->type)) {
    throw LocError::Create(stmt->expr->Begin(), "mismatched assign type");
  }
}

std::shared_ptr<Ty> TyInfer::InferExpr(const std::unique_ptr<ast::Expr>& expr) {
  switch (expr->ExprKind()) {
    case ast::Expr::Kind::LIT: {
      auto& lit = (std::unique_ptr<ast::Lit>&)expr;
      switch (lit->LitKind()) {
        case ast::Lit::Kind::BOOL:
          return RecordType(lit, kTypeBool);
        case ast::Lit::Kind::STRING:
          return RecordType(lit, kTypeString);
        case ast::Lit::Kind::CHAR:
        case ast::Lit::Kind::INT:
          return RecordType(lit, UntypedInt());
        case ast::Lit::Kind::FLOAT:
          return RecordType(lit, UntypedFloat());
      }
    } break;

    case ast::Expr::Kind::CALL: {
      auto& call_expr = (std::unique_ptr<ast::CallExpr>&)expr;
      auto decl = GetDecl(call_expr->ident)->AsFuncType();

      auto i = 0;
      for (auto& arg : call_expr->args) {
        auto arg_ty = InferExpr(arg);
        if (!Resolve(arg_ty, decl->args[i])) {
          throw LocError::Create(arg->Begin(), "mismatched arg ty");
        }
        i++;
      }

      return RecordType(call_expr, decl->ret);
    } break;

    case ast::Expr::Kind::IDENT: {
      auto& ident = (std::unique_ptr<ast::Ident>&)expr;
      return RecordType(ident, GetDecl(ident)->type);
    } break;

    case ast::Expr::Kind::UNARY: {
      auto& unary_expr = (std::unique_ptr<ast::UnaryExpr>&)expr;
      return RecordType(unary_expr, InferExpr(unary_expr->expr));
    } break;

    case ast::Expr::Kind::BINARY: {
      auto& binary = (std::unique_ptr<ast::BinaryExpr>&)expr;
      auto lhs_ty = InferExpr(binary->lhs);
      auto rhs_ty = InferExpr(binary->rhs);

      std::shared_ptr<Ty> operand_ty;
      if (Resolve(lhs_ty, rhs_ty)) {
        operand_ty = rhs_ty;
      } else if (Resolve(rhs_ty, lhs_ty)) {
        operand_ty = lhs_ty;
      } else {
        throw LocError::Create(binary->lhs->Begin(), "unmatch type");
      }

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
          return RecordType(binary, kTypeBool);

        case ast::BinaryOp::Op::ADD:
        case ast::BinaryOp::Op::SUB:
        case ast::BinaryOp::Op::MUL:
        case ast::BinaryOp::Op::DIV:
          if (!operand_ty->IsUntyped() && !operand_ty->IsTypedInt() &&
              !operand_ty->IsTypedFloat()) {
            throw LocError::Create(binary->Begin(),
                                   "cannot use non numeric type of binary");
          }
          return RecordType(binary, operand_ty);

        case ast::BinaryOp::Op::MOD:
          if (!operand_ty->IsUntyped() && !operand_ty->IsTypedInt()) {
            throw LocError::Create(binary->Begin(),
                                   "cannot use non numeric type of binary");
          }
          return RecordType(binary, operand_ty);
      }
    } break;
    case ast::Expr::Kind::IF:
      return InferIf((std::unique_ptr<ast::If>&)expr);
    case ast::Expr::Kind::BLOCK:
      return InferBlock((std::unique_ptr<ast::Block>&)expr);
  }
}

std::shared_ptr<Ty> TyInfer::InferIf(const std::unique_ptr<ast::If>& e) {
  auto cond_ty = InferExpr(e->cond);
  if (!Resolve(cond_ty, kTypeBool)) {
    throw LocError::Create(e->cond->Begin(), "not bool type");
  }

  auto block_ty = InferBlock(e->block);

  std::shared_ptr<Ty> all_ty;

  if (e->HasElse()) {
    auto ignore_block_ty = e->block->IsTerminating();
    auto ignore_els_ty = e->els->IsTerminating();
    std::shared_ptr<Ty> els_ty;
    if (e->IsElseIf()) {
      els_ty = InferIf((std::unique_ptr<ast::If>&)e->els);
    } else {
      els_ty = InferBlock((std::unique_ptr<ast::Block>&)e->els);
    }
    if (ignore_block_ty) {
      all_ty = els_ty;
    } else if (ignore_els_ty) {
      all_ty = block_ty;
    } else {
      if (Resolve(block_ty, els_ty)) {
        all_ty = els_ty;
      } else if (Resolve(els_ty, block_ty)) {
        all_ty = block_ty;
      } else {
        throw LocError::Create(e->Begin(), "unmatched if branches types");
      }
    }
  } else {
    all_ty = kTypeVoid;
  }
  return RecordType(e, all_ty);
}

std::shared_ptr<Ty> TyInfer::InferBlock(const std::unique_ptr<ast::Block>& e) {
  std::shared_ptr<Ty> ty = kTypeVoid;
  for (auto& stmt : e->stmts) {
    auto stmt_ty = InferStmt(stmt);
    ty = stmt_ty;
  }
  return RecordType(e, ty);
}

}  // namespace felis

