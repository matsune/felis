#include "check/lower.h"

#include <assert.h>

#include "check/constant.h"
#include "error/error.h"
#include "unique.h"

namespace felis {

namespace {

hir::Unary::Op un_op_ast_to_hir(ast::UnaryOp::Op op) {
  switch (op) {
    case ast::UnaryOp::Op::NEG:
      return hir::Unary::Op::NEG;
    case ast::UnaryOp::Op::NOT:
      return hir::Unary::Op::NOT;
  }
}

hir::Binary::Op bin_op_ast_to_hir(ast::BinaryOp::Op op) {
  switch (op) {
    case ast::BinaryOp::Op::LT:
      return hir::Binary::Op::LT;
    case ast::BinaryOp::Op::LE:
      return hir::Binary::Op::LE;
    case ast::BinaryOp::Op::GT:
      return hir::Binary::Op::GT;
    case ast::BinaryOp::Op::GE:
      return hir::Binary::Op::GE;
    case ast::BinaryOp::Op::ADD:
      return hir::Binary::Op::ADD;
    case ast::BinaryOp::Op::SUB:
      return hir::Binary::Op::SUB;
    case ast::BinaryOp::Op::MUL:
      return hir::Binary::Op::MUL;
    case ast::BinaryOp::Op::DIV:
      return hir::Binary::Op::DIV;
    case ast::BinaryOp::Op::MOD:
      return hir::Binary::Op::MOD;
  }
}

}  // namespace

std::unique_ptr<hir::File> Lower::Lowering(std::unique_ptr<ast::File> file) {
  auto hir_file = std::make_unique<hir::File>();
  while (!file->externs.empty()) {
    auto ext = file->externs.move_front();
    auto decl = ast_decl_.at(ext.get());
    hir_file->externs.push_back(
        std::make_unique<hir::Extern>(ext->Begin(), ext->End(), decl));
  }
  while (!file->fn_decls.empty()) {
    auto fn = file->fn_decls.move_front();
    auto decl = ast_decl_.at(fn.get());
    current_func_ = decl->AsFuncType();

    std::deque<std::shared_ptr<Decl>> args;
    for (auto& arg : fn->proto->args->list) {
      args.push_back(ast_decl_.at(arg.get()));
    }
    auto block = LowerBlock(std::move(fn->block));
    block = unique_cast<hir::Block>(
        MatchBlockType(std::move(block), current_func_->ret));

    hir_file->fn_decls.push_back(std::make_unique<hir::FnDecl>(
        fn->Begin(), decl, args, std::move(block)));
  }
  return std::move(hir_file);
}

std::unique_ptr<hir::Stmt> Lower::LowerStmt(std::unique_ptr<ast::Stmt> stmt) {
  std::cout << ToString(stmt->StmtKind()) << " " << stmt.get() << std::endl;
  switch (stmt->StmtKind()) {
    case ast::Stmt::Kind::EXPR:
      return LowerExpr(unique_cast<ast::Expr>(std::move(stmt)));
    case ast::Stmt::Kind::RET:
      return LowerRet(unique_cast<ast::RetStmt>(std::move(stmt)));
    case ast::Stmt::Kind::VAR_DECL:
      return LowerVarDecl(unique_cast<ast::VarDeclStmt>(std::move(stmt)));
    case ast::Stmt::Kind::ASSIGN:
      return LowerAssign(unique_cast<ast::AssignStmt>(std::move(stmt)));
  }
}

std::unique_ptr<hir::RetStmt> Lower::LowerRet(
    std::unique_ptr<ast::RetStmt> stmt) {
  auto begin = stmt->Begin();
  if (stmt->expr) {
    auto expr = LowerExpr(std::move(stmt->expr));
    expr = MatchExprTy(std::move(expr), current_func_->ret);
    return std::make_unique<hir::RetStmt>(begin, std::move(expr));
  } else {
    return std::make_unique<hir::RetStmt>(begin);
  }
}

bool IsAllTerminated(hir::If* root) {
  assert(!root->MissingElse());
  bool else_terminated;
  if (root->IsElseBlock()) {
    auto els = (hir::Block*)root->els.get();
    else_terminated = els->HasRet();
  } else {
    auto els = (hir::If*)root->els.get();
    else_terminated = IsAllTerminated(els);
  }
  return root->block->HasRet() && else_terminated;
}

void Lower::CheckNotVoidType(const hir::Expr* expr) {
  if (!expr->Ty()->IsVoid()) return;

  switch (expr->ExprKind()) {
    case hir::Expr::Kind::IF: {
      auto& if_stmt = (std::unique_ptr<hir::If>&)expr;
      if (if_stmt->MissingElse())
        throw LocError(if_stmt->End(), "missing else clause");

      if (IsAllTerminated(if_stmt.get()))
        throw LocError(if_stmt->Begin(), "all ret if");

    } break;
    case hir::Expr::Kind::BLOCK: {
      auto& block = (std::unique_ptr<hir::Block>&)expr;
      if (block->stmts.empty()) throw LocError(block->End(), "no statements");
      if (block->HasRet())
        throw LocError(block->End(), "block contains ret statement");
    } break;
    default:
      break;
  }
  throw LocError::Create(expr->Begin(), "cannot use void type");
}

std::unique_ptr<hir::VarDeclStmt> Lower::LowerVarDecl(
    std::unique_ptr<ast::VarDeclStmt> stmt) {
  auto begin = stmt->Begin();
  auto decl = ast_decl_.at(stmt.get());
  assert(decl->type->IsUnresolved());
  auto expr = LowerExpr(std::move(stmt->expr));
  CheckNotVoidType(expr.get());
  decl->type = expr->Ty();
  decl->Debug();
  return std::make_unique<hir::VarDeclStmt>(begin, decl, std::move(expr));
}

std::unique_ptr<hir::AssignStmt> Lower::LowerAssign(
    std::unique_ptr<ast::AssignStmt> stmt) {
  auto begin = stmt->Begin();
  auto decl = ast_decl_.at(stmt.get());
  auto expr = LowerExpr(std::move(stmt->expr));
  expr = MatchExprTy(std::move(expr), decl->type);
  return std::make_unique<hir::AssignStmt>(begin, decl, std::move(expr));
}

std::unique_ptr<hir::Expr> Lower::LowerExpr(std::unique_ptr<ast::Expr> expr) {
  switch (expr->ExprKind()) {
    case ast::Expr::Kind::LIT:
      return LowerLit(unique_cast<ast::Lit>(std::move(expr)));

    case ast::Expr::Kind::CALL: {
      auto call = unique_cast<ast::CallExpr>(std::move(expr));
      auto begin = call->Begin();
      auto end = call->End();
      auto decl = ast_decl_.at(call.get());
      auto func_decl = decl->AsFuncType();
      unique_deque<hir::Expr> args;
      while (!call->args.empty()) {
        auto arg = call->args.move_front();
        auto expr = LowerExpr(std::move(arg));
        auto idx = args.size();
        auto ty = func_decl->args[idx];
        expr = MatchExprTy(std::move(expr), ty);
        args.push_back(std::move(expr));
      }
      return std::make_unique<hir::Call>(begin, end, decl, std::move(args));
    } break;

    case ast::Expr::Kind::IDENT: {
      auto decl = ast_decl_.at(expr.get());
      auto begin = expr->Begin();
      auto end = expr->End();
      return std::make_unique<hir::Variable>(begin, end, decl);
    } break;

    case ast::Expr::Kind::UNARY: {
      auto unary = unique_cast<ast::UnaryExpr>(std::move(expr));
      auto begin = unary->Begin();
      auto end = unary->End();
      auto op = un_op_ast_to_hir(unary->op->op);
      auto expr = LowerExpr(std::move(unary->expr));
      if (expr->IsConstant()) {
        return ConstUnary(unique_cast<hir::Constant>(std::move(expr)), op);
      }
      return std::make_unique<hir::Unary>(begin, end, op, std::move(expr));
    } break;

    case ast::Expr::Kind::BINARY: {
      auto binary = unique_cast<ast::BinaryExpr>(std::move(expr));
      auto begin = binary->Begin();
      auto end = binary->End();
      auto op = bin_op_ast_to_hir(binary->op->op);
      auto lhs = LowerExpr(std::move(binary->lhs));
      auto rhs = LowerExpr(std::move(binary->rhs));
      return CheckBinary(std::make_unique<hir::Binary>(
          begin, end, op, std::move(lhs), std::move(rhs)));
    } break;
    case ast::Expr::Kind::IF:
      return LowerIf(unique_cast<ast::If>(std::move(expr)));
    case ast::Expr::Kind::BLOCK:
      return LowerBlock(unique_cast<ast::Block>(std::move(expr)));
  }
}

std::unique_ptr<hir::If> Lower::LowerIf(std::unique_ptr<ast::If> expr) {
  auto begin = expr->Begin();
  auto end = expr->End();
  auto cond = LowerExpr(std::move(expr->cond));
  if (!cond->Ty()->IsBool()) {
    throw LocError::Create(cond->Begin(), "non bool if cond");
  }
  auto block = LowerBlock(std::move(expr->block));

  if (expr->HasElse()) {
    if (expr->IsElseIf()) {
      auto els = LowerIf(unique_cast<ast::If>(std::move(expr->els)));
      return std::make_unique<hir::If>(begin, end, std::move(cond),
                                       std::move(block), std::move(els));
    } else if (expr->IsElseBlock()) {
      auto els = LowerBlock(unique_cast<ast::Block>(std::move(expr->els)));
      return std::make_unique<hir::If>(begin, end, std::move(cond),
                                       std::move(block), std::move(els));
    }
  }
  return std::make_unique<hir::If>(begin, end, std::move(cond),
                                   std::move(block));
}

std::unique_ptr<hir::Block> Lower::LowerBlock(
    std::unique_ptr<ast::Block> block) {
  Loc begin = block->Begin();
  Loc end = block->End();
  unique_deque<hir::Stmt> stmts;
  while (!block->stmts.empty()) {
    stmts.push_back(LowerStmt(block->stmts.move_front()));
  }
  return std::make_unique<hir::Block>(begin, end, std::move(stmts));
}

std::unique_ptr<hir::Constant> Lower::LowerLit(std::unique_ptr<ast::Lit> lit) {
  auto begin = lit->Begin();
  auto end = lit->End();
  switch (lit->LitKind()) {
    case ast::Lit::Kind::INT:
      return ParseInt(std::move(lit));

    case ast::Lit::Kind::FLOAT: {
      auto val = ParseFloat(std::move(lit));
      return std::make_unique<hir::FloatConstant>(begin, end, val);
    } break;
    case ast::Lit::Kind::BOOL: {
      return std::make_unique<hir::BoolConstant>(begin, end,
                                                 lit->val == "true");
    } break;
    case ast::Lit::Kind::CHAR: {
      std::stringstream ss(lit->val);
      rune r;
      ss >> r;
      return std::make_unique<hir::CharConstant>(begin, end, r);
    } break;
    case ast::Lit::Kind::STRING: {
      return std::make_unique<hir::StringConstant>(begin, end, lit->val);
    } break;
  }
}

std::unique_ptr<hir::IntConstant> Lower::ParseInt(
    std::unique_ptr<ast::Lit> lit) {
  try {
    // TODO: parse int
    long long n = stoll(lit->val);
    if (n <= INT64_MAX) {
      return std::make_unique<hir::IntConstant>(lit->Begin(), lit->End(), n);
    } else {
      throw LocError::Create(lit->Begin(), "overflow int64");
    }
  } catch (std::out_of_range e) {
    throw LocError::Create(lit->Begin(), "out of range");
  } catch (std::invalid_argument e) {
    throw LocError::Create(lit->Begin(), "invalid or unimplemented");
  }
}

double Lower::ParseFloat(std::unique_ptr<ast::Lit> lit) {
  try {
    // TODO: parse float
    double n = stod(lit->val);
    return n;
  } catch (std::out_of_range e) {
    throw LocError::Create(lit->Begin(), "out of range");
  } catch (std::invalid_argument e) {
    throw LocError::Create(lit->Begin(), "invalid or unimplemented");
  }
}

bool no_check_constant_type(Type::Kind ty) {
  return ty == Type::Kind::BOOL || ty == Type::Kind::STRING;
}

bool is_left_prior_constant_type(std::shared_ptr<Type> l,
                                 std::shared_ptr<Type> r) {
  if (no_check_constant_type(l->TypeKind()) ||
      no_check_constant_type(r->TypeKind())) {
    return true;
  }

  switch (l->TypeKind()) {
    case Type::Kind::I32:
      switch (r->TypeKind()) {
        case Type::Kind::CHAR:
        case Type::Kind::I32:
          return true;
        case Type::Kind::F32:
        case Type::Kind::I64:
        case Type::Kind::F64:
          return false;
        default:
          UNREACHABLE
      }
    case Type::Kind::F32:
      switch (r->TypeKind()) {
        case Type::Kind::CHAR:
        case Type::Kind::I32:
        case Type::Kind::F32:
          return true;
        case Type::Kind::I64:
        case Type::Kind::F64:
          return false;
        default:
          UNREACHABLE
      }
    case Type::Kind::I64:
      switch (r->TypeKind()) {
        case Type::Kind::CHAR:
        case Type::Kind::I32:
        case Type::Kind::F32:
        case Type::Kind::I64:
          return true;
        case Type::Kind::F64:
          return false;
        default:
          UNREACHABLE
      }
    case Type::Kind::F64:
      switch (r->TypeKind()) {
        case Type::Kind::CHAR:
        case Type::Kind::I32:
        case Type::Kind::F32:
        case Type::Kind::I64:
        case Type::Kind::F64:
          return true;
        default:
          UNREACHABLE
      }
    default:
      UNREACHABLE
  }
}

std::unique_ptr<hir::Constant> Lower::ConstBinary(
    std::unique_ptr<hir::Binary> binary) {
  if (is_left_prior_constant_type(binary->lhs->Ty(), binary->rhs->Ty())) {
    binary->rhs = MatchExprTy(std::move(binary->rhs), binary->lhs->Ty());
  } else {
    binary->lhs = MatchExprTy(std::move(binary->lhs), binary->rhs->Ty());
  }

  auto begin = binary->Begin();
  auto end = binary->End();

  switch (binary->lhs->Ty()->TypeKind()) {
    case Type::Kind::CHAR: {
      auto l = unique_cast<hir::CharConstant>(std::move(binary->lhs));
      auto r = unique_cast<hir::CharConstant>(std::move(binary->rhs));
      switch (binary->op) {
        case hir::Binary::Op::LT:
          return std::make_unique<hir::BoolConstant>(begin, end,
                                                     l->val < r->val);
        case hir::Binary::Op::LE:
          return std::make_unique<hir::BoolConstant>(begin, end,
                                                     l->val <= r->val);
        case hir::Binary::Op::GT:
          return std::make_unique<hir::BoolConstant>(begin, end,
                                                     l->val > r->val);
        case hir::Binary::Op::GE:
          return std::make_unique<hir::BoolConstant>(begin, end,
                                                     l->val >= r->val);
        case hir::Binary::Op::ADD:
          return std::make_unique<hir::IntConstant>(begin, end,
                                                    l->val + r->val);
        case hir::Binary::Op::SUB:
          return std::make_unique<hir::IntConstant>(begin, end,
                                                    l->val - r->val);

        case hir::Binary::Op::MUL:
          return std::make_unique<hir::IntConstant>(begin, end,
                                                    l->val * r->val);
        case hir::Binary::Op::DIV:
          return std::make_unique<hir::IntConstant>(begin, end,
                                                    l->val / r->val);
        case hir::Binary::Op::MOD:
          return std::make_unique<hir::IntConstant>(begin, end,
                                                    l->val % r->val);
      }

    } break;

    case Type::Kind::STRING: {
      auto l = unique_cast<hir::StringConstant>(std::move(binary->lhs));
      auto r = unique_cast<hir::StringConstant>(std::move(binary->rhs));
      throw CompileError::Create("cannot use string for binary");
    } break;

    case Type::Kind::BOOL: {
      auto l = unique_cast<hir::BoolConstant>(std::move(binary->lhs));
      auto r = unique_cast<hir::BoolConstant>(std::move(binary->rhs));
      throw CompileError::Create("cannot use bool for binary");
    } break;

    case Type::Kind::I32:
    case Type::Kind::I64: {
      auto l = unique_cast<hir::IntConstant>(std::move(binary->lhs));
      auto r = unique_cast<hir::IntConstant>(std::move(binary->rhs));
      switch (binary->op) {
        case hir::Binary::Op::LT:
          return std::make_unique<hir::BoolConstant>(begin, end,
                                                     l->val < r->val);
        case hir::Binary::Op::LE:
          return std::make_unique<hir::BoolConstant>(begin, end,
                                                     l->val <= r->val);
        case hir::Binary::Op::GT:
          return std::make_unique<hir::BoolConstant>(begin, end,
                                                     l->val > r->val);
        case hir::Binary::Op::GE:
          return std::make_unique<hir::BoolConstant>(begin, end,
                                                     l->val >= r->val);
        case hir::Binary::Op::ADD:
          return std::make_unique<hir::IntConstant>(begin, end,
                                                    l->val + r->val);
        case hir::Binary::Op::SUB:
          return std::make_unique<hir::IntConstant>(begin, end,
                                                    l->val - r->val);

        case hir::Binary::Op::MUL:
          return std::make_unique<hir::IntConstant>(begin, end,
                                                    l->val * r->val);
        case hir::Binary::Op::DIV:
          return std::make_unique<hir::IntConstant>(begin, end,
                                                    l->val / r->val);
        case hir::Binary::Op::MOD:
          return std::make_unique<hir::IntConstant>(begin, end,
                                                    l->val % r->val);
      }

    } break;
    case Type::Kind::F32:
    case Type::Kind::F64: {
      auto l = unique_cast<hir::FloatConstant>(std::move(binary->lhs));
      auto r = unique_cast<hir::FloatConstant>(std::move(binary->rhs));
      switch (binary->op) {
        case hir::Binary::Op::LT:
          return std::make_unique<hir::BoolConstant>(begin, end,
                                                     l->val < r->val);
        case hir::Binary::Op::LE:
          return std::make_unique<hir::BoolConstant>(begin, end,
                                                     l->val <= r->val);
        case hir::Binary::Op::GT:
          return std::make_unique<hir::BoolConstant>(begin, end,
                                                     l->val > r->val);
        case hir::Binary::Op::GE:
          return std::make_unique<hir::BoolConstant>(begin, end,
                                                     l->val >= r->val);
        case hir::Binary::Op::ADD:
          return std::make_unique<hir::IntConstant>(begin, end,
                                                    l->val + r->val);
        case hir::Binary::Op::SUB:
          return std::make_unique<hir::IntConstant>(begin, end,
                                                    l->val - r->val);

        case hir::Binary::Op::MUL:
          return std::make_unique<hir::IntConstant>(begin, end,
                                                    l->val * r->val);
        case hir::Binary::Op::DIV:
          return std::make_unique<hir::IntConstant>(begin, end,
                                                    l->val / r->val);
        case hir::Binary::Op::MOD:
          throw LocError::Create(begin,
                                 "operator \% not defined on untyped float");
      }

    } break;
      break;
    default:
      UNREACHABLE
  }
}

inline bool is_operand_type(Type::Kind ty) {
  return ty == Type::Kind::I32 || ty == Type::Kind::I64 ||
         ty == Type::Kind::F32 || ty == Type::Kind::F64 ||
         ty == Type::Kind::BOOL || ty == Type::Kind::CHAR ||
         ty == Type::Kind::STRING;
}

std::unique_ptr<hir::Expr> Lower::CheckBinary(
    std::unique_ptr<hir::Binary> binary) {
  if (!is_operand_type(binary->lhs->Ty()->TypeKind())) {
    throw LocError::Create(binary->lhs->Begin(), "not binary type");
  }
  if (!is_operand_type(binary->rhs->Ty()->TypeKind())) {
    throw LocError::Create(binary->lhs->Begin(), "not binary type");
  }

  if (binary->lhs->IsConstant() && binary->rhs->IsConstant()) {
    return ConstBinary(std::move(binary));
  }
  std::shared_ptr<Type> operand_ty;
  if (binary->lhs->IsConstant()) {
    operand_ty = binary->rhs->Ty();
    binary->lhs = MatchExprTy(std::move(binary->lhs), operand_ty);
  } else {
    operand_ty = binary->lhs->Ty();
    binary->rhs = MatchExprTy(std::move(binary->rhs), operand_ty);
  }

  switch (binary->op) {
    case hir::Binary::Op::LT:
    case hir::Binary::Op::LE:
    case hir::Binary::Op::GT:
    case hir::Binary::Op::GE:
    case hir::Binary::Op::ADD:
    case hir::Binary::Op::SUB:
    case hir::Binary::Op::MUL:
    case hir::Binary::Op::DIV:
      if (!operand_ty->IsNumeric())
        throw LocError::Create(binary->Begin(), "not numeric type");
      break;
    case hir::Binary::Op::MOD:
      if (operand_ty->IsFloat())
        throw LocError::Create(binary->lhs->Begin(),
                               "operator \% not defined on float");
      break;
  }

  return std::move(binary);
}

std::unique_ptr<hir::Constant> Lower::ConstUnary(
    std::unique_ptr<hir::Constant> cons, hir::Unary::Op op) {
  switch (op) {
    case hir::Unary::Op::NOT:
      if (cons->Ty()->IsBool()) {
        auto b = unique_cast<hir::BoolConstant>(std::move(cons));
        b->val = !b->val;
        return std::move(b);
      } else {
        throw LocError::Create(cons->Begin(), "non bool type");
      }
    case hir::Unary::Op::NEG:
      if (cons->Ty()->IsNumeric()) {
        if (cons->Ty()->IsI32() || cons->Ty()->IsI64()) {
          auto b = unique_cast<hir::IntConstant>(std::move(cons));
          b->val = -b->val;
          return b;
        } else {
          auto b = unique_cast<hir::FloatConstant>(std::move(cons));
          b->val = -b->val;
          return b;
        }
      } else {
        throw LocError::Create(cons->Begin(), "non numeric type");
      }
  }
}

std::unique_ptr<hir::Expr> Lower::MatchExprTy(std::unique_ptr<hir::Expr> e,
                                              std::shared_ptr<Type> ty) {
  std::cout << "MatchExpr " << e.get() << " " << ToString(ty.get())
            << std::endl;
  switch (e->ExprKind()) {
    case hir::Expr::Kind::VALUE:
      return MatchValueType(unique_cast<hir::Value>(std::move(e)), ty);

    case hir::Expr::Kind::BINARY:
    case hir::Expr::Kind::CALL:
    case hir::Expr::Kind::UNARY:
      return MatchJustType(std::move(e), ty);

    case hir::Expr::Kind::IF:
      return MatchIfType(unique_cast<hir::If>(std::move(e)), ty);

    case hir::Expr::Kind::BLOCK:
      return MatchBlockType(unique_cast<hir::Block>(std::move(e)), ty);
  };
}

std::unique_ptr<hir::Expr> Lower::MatchJustType(std::unique_ptr<hir::Expr> e,
                                                std::shared_ptr<Type> ty) {
  if (*e->Ty() != *ty) {
    throw LocError::Create(e->Begin(), "unmatched expr type");
  }
  return std::move(e);
}

std::unique_ptr<hir::Expr> Lower::MatchValueType(std::unique_ptr<hir::Value> e,
                                                 std::shared_ptr<Type> ty) {
  switch (e->ValueKind()) {
    case hir::Value::Kind::VARIABLE:
      return MatchJustType(std::move(e), ty);
    case hir::Value::Kind::CONSTANT:
      return MatchConstantType(unique_cast<hir::Constant>(std::move(e)), ty);
  }
}

std::unique_ptr<hir::Expr> Lower::MatchConstantType(
    std::unique_ptr<hir::Constant> c, std::shared_ptr<Type> ty) {
  switch (c->ConstantKind()) {
    case hir::Constant::Kind::CHAR:
      return MatchType(unique_cast<hir::CharConstant>(std::move(c)), ty);
    case hir::Constant::Kind::INT:
      return MatchType(unique_cast<hir::IntConstant>(std::move(c)), ty);
    case hir::Constant::Kind::BOOL:
      return MatchType(unique_cast<hir::BoolConstant>(std::move(c)), ty);
    case hir::Constant::Kind::FLOAT:
      return MatchType(unique_cast<hir::FloatConstant>(std::move(c)), ty);
    case hir::Constant::Kind::STRING:
      return MatchType(unique_cast<hir::StringConstant>(std::move(c)), ty);
  }
}

std::unique_ptr<hir::Expr> Lower::MatchIfType(std::unique_ptr<hir::If> e,
                                              std::shared_ptr<Type> ty) {
  e->block = unique_cast<hir::Block>(MatchExprTy(std::move(e->block), ty));
  if (e->IsElseIf()) {
    e->els = unique_cast<hir::If>(MatchExprTy(std::move(e->els), ty));
  } else if (e->IsElseBlock()) {
    e->els = unique_cast<hir::Block>(MatchExprTy(std::move(e->els), ty));
  }
  return std::move(e);
}

std::unique_ptr<hir::Expr> Lower::MatchBlockType(std::unique_ptr<hir::Block> e,
                                                 std::shared_ptr<Type> ty) {
  if (e->HasRet()) {
    // ret block doesn't need to check type
    return std::move(e);
  }
  if (*e->Ty() == *ty) return std::move(e);

  if (e->stmts.empty()) throw LocError(e->End(), "block is empty");

  auto last = e->stmts.move_back();
  last = MatchExprTy(unique_cast<hir::Expr>(std::move(last)), ty);
  e->stmts.push_back(std::move(last));
  return std::move(e);
}

}  // namespace felis
