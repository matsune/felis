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

    if (!current_func_->ret->IsVoid() && !fn_decl->block->IsTerminating()) {
      throw LocError::Create(fn_decl->End(), "func block needs ret stmt");
    }
    InferBlock(fn_decl->block, false);
  }
}

//
// Infer a type of `ast::Stmt`.
// `as_expr` means whether the statement would be used as a value
// for other statement. For example, block is a kind of expressions
// but it consists of a list of statements, and the last statement
// may be a value of the block.
//
// ```
// hoge()
//
// let a = {
//    let b = 2
//    fuga(b)
// }
// ```
// In this case, `hoge()` is just a statement because result value
// won't be used. However, `fuga(b)` is a statement but also a value
// of block, this means the result value will be assigned into
// variable `a`.
//
std::shared_ptr<Ty> TyInfer::InferStmt(const std::unique_ptr<ast::Stmt>& stmt,
                                       bool as_expr) {
  if (as_expr) {
    switch (stmt->StmtKind()) {
      case ast::Stmt::Kind::VAR_DECL:
      case ast::Stmt::Kind::ASSIGN:
        throw LocError::Create(stmt->Begin(), "not value return statement");
      default:
        break;
    }
  }

  switch (stmt->StmtKind()) {
    case ast::Stmt::Kind::EXPR:
      return InferExpr((std::unique_ptr<ast::Expr>&)stmt, as_expr);
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
    auto expr_ty = InferExpr(stmt->expr, true);
    if (!Resolve(expr_ty, current_func_->ret)) {
      throw LocError::Create(stmt->Begin(), "mismatched ret type");
    }
  }
}

void TyInfer::InferVarDecl(const std::unique_ptr<ast::VarDeclStmt>& stmt) {
  auto decl = GetDecl(stmt->name);
  auto expr_ty = InferExpr(stmt->expr, true);
  if (expr_ty->IsVoid()) {
    throw LocError::Create(stmt->Begin(), "cannot decl void type var");
  }
  assert(decl->type == nullptr);
  decl->type = expr_ty;
}

void TyInfer::InferAssign(const std::unique_ptr<ast::AssignStmt>& stmt) {
  auto decl = GetDecl(stmt->name);
  auto expr_ty = InferExpr(stmt->expr, true);
  if (!Resolve(expr_ty, decl->type)) {
    throw LocError::Create(stmt->expr->Begin(), "mismatched assign type");
  }
}

std::shared_ptr<Ty> TyInfer::InferExpr(const std::unique_ptr<ast::Expr>& expr,
                                       bool as_expr) {
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
        auto arg_ty = InferExpr(arg, true);
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
      return RecordType(unary_expr, InferExpr(unary_expr->expr, true));
    } break;

    case ast::Expr::Kind::BINARY: {
      auto& binary = (std::unique_ptr<ast::BinaryExpr>&)expr;
      auto lhs_ty = InferExpr(binary->lhs, true);
      auto rhs_ty = InferExpr(binary->rhs, true);

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
      return InferIf((std::unique_ptr<ast::If>&)expr, as_expr);
    case ast::Expr::Kind::BLOCK:
      return InferBlock((std::unique_ptr<ast::Block>&)expr, as_expr);
  }
}

std::shared_ptr<Ty> TyInfer::InferIf(const std::unique_ptr<ast::If>& e,
                                     bool as_expr) {
  auto cond_ty = InferExpr(e->cond, true);
  if (!Resolve(cond_ty, kTypeBool)) {
    throw LocError::Create(e->cond->Begin(), "if condition must be bool type");
  }

  auto block_ty = InferBlock(e->block, as_expr);

  std::shared_ptr<Ty> whole_ty = kTypeVoid;

  if (e->HasElse()) {
    auto is_then_terminating = e->block->IsTerminating();
    auto is_else_terminating = e->els->IsTerminating();

    std::shared_ptr<Ty> els_ty;
    if (e->IsElseIf()) {
      els_ty = InferIf((std::unique_ptr<ast::If>&)e->els, as_expr);
    } else {
      els_ty = InferBlock((std::unique_ptr<ast::Block>&)e->els, as_expr);
    }

    if (as_expr) {
      if (is_then_terminating && is_else_terminating) {
        // Cannot infer type like this
        //
        // ```
        // let a = if true {
        //    ret 3
        //  } else {
        //    ret 4
        //  }
        // ```
        //
        throw LocError::Create(e->Begin(), "returning in all branch");
      } else if (is_then_terminating) {
        whole_ty = els_ty;
      } else if (is_else_terminating) {
        whole_ty = block_ty;
      } else {
        // Both of then block and else block should be same type
        if (Resolve(block_ty, els_ty)) {
          whole_ty = els_ty;
        } else if (Resolve(els_ty, block_ty)) {
          whole_ty = block_ty;
        } else {
          throw LocError::Create(e->Begin(), "unmatched if branches types");
        }
      }
    }
  } else {
    if (as_expr) {
      // No else if-statement can't be used as value
      //
      // ex) let a = if true { 4 }
      //
      throw LocError::Create(e->Begin(), "incomplete if statement");
    }
  }
  return RecordType(e, whole_ty);
}

std::shared_ptr<Ty> TyInfer::InferBlock(const std::unique_ptr<ast::Block>& e,
                                        bool as_expr) {
  std::shared_ptr<Ty> ty = kTypeVoid;
  for (auto it = e->stmts.begin(); it != e->stmts.end(); it++) {
    // If the block will be the value of parent statement,
    // last statement of this block will also be the value.
    // For example, below block will be the value of `let a`
    // and `b` will be a value of whole this block.
    //
    // ```
    // let a = {  // block is expr for `let a`
    //  let b = 2
    //  b         // last statement substitutes expr for block
    // }
    // ```
    bool is_last = std::next(it) == e->stmts.end();
    ty = InferStmt(*it, is_last && as_expr);
  }
  if (!as_expr) {
    ty = kTypeVoid;
  }
  return RecordType(e, ty);
}

}  // namespace felis

