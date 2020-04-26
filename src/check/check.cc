#include "check/check.h"

#include <assert.h>

#include "error/error.h"

namespace felis {

void Checker::SetupBuiltin() {
  assert(currentScope_->IsTop());
  // insert basic types into global scope
  currentScope_->InsertType("void", std::make_shared<Type>(Type::Kind::VOID));
  currentScope_->InsertType("i32", std::make_shared<Type>(Type::Kind::I32));
  currentScope_->InsertType("bool", std::make_shared<Type>(Type::Kind::BOOL));
  currentScope_->InsertType("char", std::make_shared<Type>(Type::Kind::CHAR));
}

void Checker::Check(std::unique_ptr<ast::File>& file) {
  for (auto& ext : file->externs) {
    auto decl = InsertFnDecl(true, ext->proto);
    RecordNodeDecl(ext.get(), decl);
  }
  for (auto& fn : file->fnDecls) {
    auto decl = InsertFnDecl(false, fn->proto);
    RecordNodeDecl(fn.get(), decl);
  }

  for (auto& fn : file->fnDecls) {
    CheckFnDecl(fn);
  }
}

void Checker::CheckFnDecl(std::unique_ptr<ast::FnDecl>& fnDecl) {
  auto decl = node_decl_[fnDecl.get()];
  decl->Debug();
  currentFunc_ = decl;

  OpenScope();
  for (auto& arg : fnDecl->proto->args) {
    // arg-name duplication is already checked in parser
    auto argDecl = std::make_shared<Decl>(
        arg->name->sval, LookupType(arg->ty->sval), Decl::Kind::ARG);
    currentScope_->InsertDecl(arg->name->sval, argDecl);
  }
  for (auto& stmt : fnDecl->block->stmts) {
    CheckStmt(stmt);
  }
  CloseScope();
}

void Checker::CheckStmt(std::unique_ptr<ast::Stmt>& stmt) {
  switch (stmt->StmtKind()) {
    case ast::Stmt::Kind::EXPR: {
      throw CompileError::CreatePosFmt(stmt->GetPos(), "unimplemented expr");
      auto expr = (ast::Expr*)stmt.get();
    } break;

    case ast::Stmt::Kind::RET: {
      std::cout << "ret ";
      auto retStmt = (ast::RetStmt*)stmt.get();
      auto funcType = currentFunc_->AsFuncType();
      if (retStmt->expr) {
        auto expr = MakeExpr(retStmt->expr.get());
        if (*expr->Ty() != *funcType->ret) TryExpTy(expr.get(), funcType->ret);
        expr->Debug();
      } else {
        // empty return
        if (!funcType->ret->IsVoid()) {
          throw CompileError::CreatePosFmt(retStmt->GetPos(),
                                           "func type is not void");
        }
        std::cout << " void" << std::endl;
      }
    } break;

    case ast::Stmt::Kind::VAR_DECL: {
      std::cout << "varDecl ";
      auto declStmt = (ast::VarDeclStmt*)stmt.get();
      std::string name = declStmt->name->sval;
      if (!CanDecl(name)) {
        throw CompileError::CreatePosFmt(declStmt->GetPos(),
                                         "redeclared var %s", name.c_str());
      }
      auto exp = MakeExpr(declStmt->expr.get());
      auto decl = std::make_shared<Decl>(
          name, exp->Ty(), declStmt->isLet ? Decl::Kind::LET : Decl::Kind::VAR);
      RecordNodeDecl(declStmt, decl);
      decl->Debug();

    } break;

    case ast::Stmt::Kind::ASSIGN: {
      throw CompileError::CreatePosFmt(stmt->GetPos(), "unimplemented assign");
      /* auto assign = (AssignStmt*)stmt.get(); */
      /* auto defVar = sm_.LookupVariable(assign->name->sval); */
      /* if (!defVar) { */
      /*   throw CompileError::CreatePosFmt(assign->GetPos(), "undeclared var
       * %s", */
      /*                                    *assign->name->sval.c_str()); */
      /* } */
      /* if (!defVar->IsMut()) { */
      /*   throw CompileError::CreatePosFmt(assign->GetPos(), */
      /*                                    "variable %s is mutable", */
      /*                                    assign->name->sval.c_str()); */
      /* } */
      /* Ty ty; */
      /* llvm::Value* value; */
      /* Build(assign->expr.get(), value, ty); */
      /* if (defVar->ty != ty) { */
      /*   throw CompileError::CreatePosFmt(assign->expr->GetPos(), */
      /*                                    "assigned expr type doesn't match");
       */
      /* } */

      /* builder_.CreateStore(value, defVar->Value()); */
    } break;

    case ast::Stmt::Kind::IF: {
      throw CompileError::CreatePosFmt(stmt->GetPos(), "unimplemented if");
      /* auto ifStmt = (IfStmt*)stmt.get(); */
      /* Ty condTy; */
      /* llvm::Value* condValue; */
      /* Build(ifStmt->cond.get(), condValue, condTy); */
      /* if (condTy != Ty::BOOL) { */
      /*   throw CompileError::CreatePosFmt(ifStmt->cond->GetPos(), */
      /*                                    "non bool if cond"); */
      /* } */
      /* llvm::Value* cond = */
      /*     builder_.CreateICmpNE(condValue,
       * llvm::ConstantInt::getFalse(ctx_)); */

      /* llvm::BasicBlock* thenBB = */
      /*     llvm::BasicBlock::Create(ctx_, "then", currentFn_->func); */
      /* llvm::BasicBlock* elseBB = */
      /*     llvm::BasicBlock::Create(ctx_, "else", currentFn_->func); */
      /* builder_.CreateCondBr(cond, thenBB, elseBB); */
      /* builder_.SetInsertPoint(thenBB); */

      /* sm_.Push(); */

      /* Build(ifStmt->block.get()); */

      /* sm_.Pop(); */

      /* builder_.CreateBr(elseBB); */
      /* builder_.SetInsertPoint(elseBB); */

      /* if (ifStmt->els) { */
      /*   if (ifStmt->els->StmtKind() == Stmt::Kind::IF) { */
      /*     auto elsStmt = (IfStmt*)ifStmt->els.get(); */
      /*     Build(ifStmt->els); */
      /*   } else if (ifStmt->els->StmtKind() == Stmt::Kind::BLOCK) { */
      /*     auto elsBlock = (Block*)ifStmt->els.get(); */
      /*     sm_.Push(); */
      /*     Build(elsBlock); */
      /*     sm_.Pop(); */
      /*   } */
      /* } */
    } break;
    case ast::Stmt::Kind::BLOCK: {
      throw CompileError::CreatePosFmt(stmt->GetPos(), "unimplemented block");
      /* auto block = (Block*)stmt.get(); */
      /* sm_.Push(); */

      /* Build(block); */

      /* sm_.Pop(); */
    } break;
  }
}

std::unique_ptr<hir::Expr> Checker::MakeExpr(ast::Expr* expr) {
  switch (expr->ExprKind()) {
    case ast::Expr::Kind::LIT: {
      auto lit = (ast::Lit*)expr;
      return MakeLit(lit);
    } break;

    case ast::Expr::Kind::CALL: {
      auto callExpr = (ast::CallExpr*)expr;

      auto decl = LookupDecl(callExpr->ident->sval);
      if (decl == nullptr) {
        throw CompileError::CreatePosFmt(callExpr->GetPos(),
                                         "undefined function %s",
                                         callExpr->ident->sval.c_str());
      }
      if (!decl->IsFunc()) {
        throw CompileError::CreatePosFmt(callExpr->GetPos(),
                                         "%s is not declared as function",
                                         callExpr->ident->sval.c_str());
      }
      auto fnType = (FuncType*)decl->type.get();
      if (fnType->args.size() != callExpr->args.size()) {
        throw CompileError::CreatePosFmt(callExpr->GetPos(),
                                         "args count doesn't match");
      }
      auto call = std::make_unique<hir::Call>(callExpr->GetPos());
      call->decl = decl;
      for (int i = 0; i < callExpr->args.size(); i++) {
        auto& arg = callExpr->args[i];
        auto exp = MakeExpr(arg.get());
        auto ty = fnType->args[i];
        TryExpTy(exp.get(), ty);
        call->argExprs.push_back(std::move(exp));
      }
      return std::move(call);
    } break;

    case ast::Expr::Kind::IDENT: {
      auto ident = (ast::Ident*)expr;
      auto decl = LookupDecl(ident->sval);
      if (decl == nullptr) {
        throw CompileError::CreatePosFmt(
            ident->GetPos(), "undefined function %s", ident->sval.c_str());
      }
      if (decl->IsFunc()) {
        throw CompileError::CreatePosFmt(ident->GetPos(),
                                         "%s is not declared as variable",
                                         ident->sval.c_str());
      }
      auto value = std::make_unique<hir::Variable>(ident->GetPos());
      value->decl = decl;
      return std::move(value);
    } break;
    case ast::Expr::Kind::UNARY: {
      auto unaryExpr = (ast::UnaryExpr*)expr;
      return std::make_unique<hir::Unary>(unaryExpr->GetPos(), unaryExpr->unOp,
                                          MakeExpr(unaryExpr->expr.get()));
    } break;
    case ast::Expr::Kind::BINARY: {
      auto binaryExpr = (ast::BinaryExpr*)expr;
      auto lhs = MakeExpr(binaryExpr->lhs.get());
      auto rhs = MakeExpr(binaryExpr->rhs.get());
      CheckBinary(lhs, rhs, binaryExpr->op);
      return std::make_unique<hir::Binary>(binaryExpr->GetPos(), binaryExpr->op,
                                           std::move(lhs), std::move(rhs));
    } break;
    default:
      return nullptr;
  }
}

bool IsAddOperandType(Type::Kind kind) {
  switch (kind) {
    case Type::Kind::I32:
    case Type::Kind::F32:
    case Type::Kind::CHAR:
      return true;
    default:
      return false;
  }
}

int NumPrior(std::shared_ptr<Type> ty) {
  if (ty->TypeKind() == Type::Kind::F32) {
    return 2;
  } else if (ty->TypeKind() == Type::Kind::I32) {
    return 1;
  } else if (ty->TypeKind() == Type::Kind::CHAR) {
    return 0;
  } else {
    exit(1);
  }
}

void Checker::CheckBinary(std::unique_ptr<hir::Expr>& lhs,
                          std::unique_ptr<hir::Expr>& rhs, ast::BinOp op) {
  auto lhsTy = lhs->Ty();
  auto rhsTy = rhs->Ty();
  switch (op) {
    case ast::BinOp::ADD: {
      if (!IsAddOperandType(lhsTy->TypeKind())) {
        throw CompileError::CreatePosFmt(lhs->pos, "cannot add lhs type");
      }
      if (!IsAddOperandType(rhsTy->TypeKind())) {
        throw CompileError::CreatePosFmt(rhs->pos, "cannot add rhs type");
      }
      auto lhsTyPri = NumPrior(lhsTy);
      auto rhsTyPri = NumPrior(rhsTy);
      if (lhsTyPri == rhsTyPri) {
        return;
      } else if (lhsTyPri < rhsTyPri) {
        // try cast lhs
        TryExpTy(lhs.get(), rhsTy);
      } else {
        // try cast rhs
        TryExpTy(rhs.get(), lhsTy);
      }
    } break;
    default:
      // TODO:
      throw CompileError::CreatePosFmt(lhs->pos, "unimplemented check ibnary");
  }
}

void Checker::TryConstantTy(hir::Constant* cons, std::shared_ptr<Type> ty) {
  switch (cons->ConstantKind()) {
    case hir::Constant::Kind::CHAR: {
      // char may be int or float
      auto charConst = (hir::CharConstant*)cons;
      switch (ty->TypeKind()) {
        case Type::CHAR:
          return;
        case Type::I32: {
          auto rune = charConst->val;
          delete charConst;
          cons = new hir::IntConstant(rune.scalar, true);
        } break;
        default:
          // TODO:
          throw CompileError::CreatePosFmt(
              cons->pos, "unimplemented charConst implicit cast");
      }
    } break;
    case hir::Constant::Kind::INT: {
      // int may be float
      // and check overflow
      auto intConst = (hir::IntConstant*)cons;
      switch (ty->TypeKind()) {
        case Type::I32:
          if (intConst->is32) return;

          if (intConst->val > INT32_MAX) {
            throw CompileError::CreatePosFmt(cons->pos, "overflow int32");
          }
          intConst->is32 = true;
          return;
        case Type::F32:
          // TODO:
          /* double fval = double(intConst->val); */
          throw CompileError::CreatePosFmt(
              cons->pos, "unimplemented floatConst implicit cast");

        default:
          throw CompileError::CreatePosFmt(cons->pos, "can't cast");
      }
    } break;
    case hir::Constant::Kind::BOOL:
      if (!ty->IsBool())
        throw CompileError::CreatePosFmt(cons->pos, "can't cast bool");
      break;
    case hir::Constant::Kind::FLOAT:
      if (!ty->IsF32())
        throw CompileError::CreatePosFmt(cons->pos, "can't cast f32");
      break;
    case hir::Constant::Kind::STRING:
      if (!ty->IsString())
        throw CompileError::CreatePosFmt(cons->pos, "can't cast string");
      break;
  }
}

// check exp's type and try to set type `ty`
void Checker::TryExpTy(hir::Expr* expr, std::shared_ptr<Type> ty) {
  switch (expr->ExprKind()) {
    case hir::Expr::Kind::VALUE: {
      auto value = (hir::Value*)expr;
      switch (value->ValueKind()) {
        case hir::Value::Kind::VARIABLE: {
          // Variables can't be casted implicitly
          auto var = (hir::Variable*)value;
          if (*var->decl->type != *ty) {
            throw CompileError::CreatePosFmt(expr->pos,
                                             "unmatched variable type");
          }
        } break;
        case hir::Value::Kind::CONSTANT: {
          // Constants can be casted implicitly
          auto cons = (hir::Constant*)value;
          TryConstantTy(cons, ty);
        } break;
      }
    }
    case hir::Expr::Kind::BINARY: {
      auto binary = (hir::Binary*)expr;
      if (*binary->Ty() == *ty) return;
      TryExpTy(binary->lhs.get(), ty);
      TryExpTy(binary->rhs.get(), ty);
      if (*binary->Ty() != *ty)
        throw CompileError::CreatePosFmt(binary->pos, "unmatched binary type");
    } break;
    default: {
      if (*expr->Ty() != *ty) {
        throw CompileError::CreatePosFmt(expr->pos, "unmatched exp type");
      }
    } break;
  }
}  // namespace felis

std::unique_ptr<hir::Constant> Checker::MakeLit(ast::Lit* lit) {
  switch (lit->LitKind()) {
    case ast::Lit::Kind::INT: {
      auto litInt = (ast::LitInt*)lit;
      return std::make_unique<hir::IntConstant>(lit->GetPos(), litInt->ival);
    } break;
    case ast::Lit::Kind::FLOAT: {
      auto litFloat = (ast::LitFloat*)lit;
      return std::make_unique<hir::FloatConstant>(lit->GetPos(),
                                                  litFloat->fval);
    } break;
    case ast::Lit::Kind::BOOL: {
      auto litBool = (ast::LitBool*)lit;
      return std::make_unique<hir::BoolConstant>(lit->GetPos(), litBool->bval);
    } break;
    case ast::Lit::Kind::CHAR: {
      auto litChar = (ast::LitChar*)lit;
      return std::make_unique<hir::CharConstant>(lit->GetPos(), litChar->cval);
    } break;
    case ast::Lit::Kind::STR: {
      auto litStr = (ast::LitStr*)lit;
      return std::make_unique<hir::StringConstant>(lit->GetPos(), litStr->sval);
    } break;
  }
}

void Checker::RecordNodeDecl(ast::Node* node, std::shared_ptr<Decl> decl) {
  node_decl_[node] = decl;
}

std::shared_ptr<Decl> Checker::InsertFnDecl(
    bool isExt, const std::unique_ptr<ast::FnProto>& proto) {
  if (!CanDecl(proto->name->sval)) {
    throw CompileError::CreatePosFmt(proto->name->GetPos(),
                                     "redeclared function %s",
                                     proto->name->sval.c_str());
  }

  std::vector<std::shared_ptr<Type>> args;
  for (auto& arg : proto->args) {
    auto ty = LookupType(arg->ty->sval);
    if (!ty) {
      throw CompileError::CreatePosFmt(arg->ty->GetPos(), "unknown arg type %s",
                                       arg->ty->sval.c_str());
    }
    args.push_back(ty);
  }
  std::shared_ptr<Type> retTy;
  if (proto->ret) {
    retTy = LookupType(proto->ret->sval);
    if (!retTy) {
      throw CompileError::CreatePosFmt(proto->ret->GetPos(),
                                       "unknown ret type %s",
                                       proto->ret->sval.c_str());
    }
  } else {
    retTy = std::make_shared<Type>(Type::Kind::VOID);
  }
  auto fnType = std::make_shared<FuncType>(std::move(args), std::move(retTy));
  Decl::Kind kind = isExt ? Decl::Kind::EXT : Decl::Kind::FN;
  auto decl = std::make_shared<Decl>(proto->name->sval, fnType, kind);
  currentScope_->InsertDecl(proto->name->sval, decl);
  return decl;
}

void Checker::OpenScope() {
  currentScope_ = std::make_shared<Scope>(currentScope_);
}

void Checker::CloseScope() {
  assert(!currentScope_->IsTop());
  currentScope_ = currentScope_->GetParent();
}

bool Checker::CanDecl(std::string name) {
  return currentScope_->FindDecl(name) == nullptr;
}

std::shared_ptr<Decl> Checker::LookupDecl(std::string name) {
  auto scope = currentScope_;
  while (scope) {
    auto def = scope->FindDecl(name);
    if (def) return def;
    scope = scope->GetParent();
  }
  return nullptr;
}

std::shared_ptr<Type> Checker::LookupType(std::string name) {
  auto scope = currentScope_;
  while (scope) {
    auto ty = scope->FindType(name);
    if (ty) return ty;
    scope = scope->GetParent();
  }
  return nullptr;
}

}  // namespace felis
