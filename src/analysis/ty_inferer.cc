#include "ty_inferer.h"

#include "string/string.h"

namespace felis {

Ty getTy(const std::unique_ptr<Ident> &ident) {
  if (!ident) return Ty::VOID;
  std::string str = ident->sval;
  if (str == "int") {
    return Ty::INT;
  } else if (str == "bool") {
    return Ty::BOOL;
  } else if (str == "string") {
    return Ty::STRING;
  } else if (str == "char") {
    return Ty::CHAR;
  } else if (str == "float") {
    return Ty::FLOAT;
  } else {
    return Ty::UNKNOWN;
  }
}

void TyInferer::InsertFnProto(NodeId nodeId, bool isExt,
                              const std::unique_ptr<FnProto> &proto) {
  std::vector<Ty> args;
  Ty retTy;
  for (auto &arg : proto->args) {
    Ty ty = getTy(arg->ty);
    if (ty == Ty::UNKNOWN) {
      handler_.Raise(arg->ty->GetPos(),
                     format("unknown arg type %s", arg->ty->sval.c_str()));
      return;
    }
    args.push_back(ty);
  }
  retTy = getTy(proto->ret);
  if (retTy == Ty::UNKNOWN) {
    handler_.Raise(proto->ret->GetPos(),
                   format("unknown ret type %s", proto->ret->sval.c_str()));
    return;
  }
  if (defTable_.IsDeclaredFn(depth_, proto->name->sval)) {
    handler_.Raise(proto->name->GetPos(),
                   format("redeclared function %s", proto->name->sval.c_str()));
    return;
  }
  auto defId = defTable_.InsertFn(std::make_unique<DefFn>(
      depth_, proto->name->sval, proto.get(), isExt, args, retTy));
  idMap_[nodeId] = defId;
}

void TyInferer::Parse(std::unique_ptr<File> &file) {
  depth_ = 0;
  for (auto &ext : file->externs) {
    InsertFnProto(ext->id, true, ext->proto);
  }
  for (auto &fn : file->fnDecls) {
    InsertFnProto(fn->id, false, fn->proto);
  }
  if (handler_.HasError()) {
    return;
  }
  std::cout << "--------------" << std::endl << "Global" << std::endl;
  defTable_.PrintGlobal();

  for (auto &fn : file->fnDecls) {
    Infer(fn);
  }
}

void TyInferer::PushScope() {
  depth_++;
  defTable_.PushLocal();
}

void TyInferer::PopScope() {
  depth_--;
  defTable_.PopLocal();
}

void TyInferer::Infer(std::unique_ptr<FnDecl> &fn) {
  // First check and insert args
  PushScope();

  DefId defId = idMap_.at(fn->id);
  currentFn_ = (DefFn *)defTable_.Get(defId);

  for (auto &arg : fn->proto->args) {
    Ty ty = getTy(arg->ty);
    if (ty == Ty::UNKNOWN) {
      handler_.Raise(arg->ty->GetPos(),
                     format("unknown arg type %s", arg->ty->sval.c_str()));
      return;
    }

    if (defTable_.IsDeclaredVar(depth_, arg->name->sval)) {
      handler_.Raise(arg->GetPos(),
                     format("redeclared var %s", arg->name->sval.c_str()));
      return;
    }
    defTable_.InsertVar(
        std::make_unique<DefVar>(depth_, arg->name->sval, arg.get(), true, ty));
  }

  if (handler_.HasError()) {
    return;
  }

  // dive into block
  Infer(fn->block.get());
  if (handler_.HasError()) {
    return;
  }

  std::cout << ">>>> " << fn->proto->name->sval << std::endl;
  defTable_.PrintLocal();

  currentFn_ = nullptr;
  PopScope();
}

void TyInferer::Infer(Block *block) {
  for (auto &stmt : block->stmts) {
    Infer(stmt);
  }
}

void TyInferer::Infer(std::unique_ptr<Stmt> &stmt) {
  switch (stmt->StmtKind()) {
    case Stmt::Kind::EXPR: {
      auto expr = (Expr *)stmt.get();
      Ty ty;
      bool ok = InferTy(ty, expr);
      if (!ok) return;
    } break;

    case Stmt::Kind::RET: {
      auto ret = (RetStmt *)stmt.get();
      Ty ty(Ty::UNKNOWN);

      if (ret->expr) {
        bool ok = InferTy(ty, ret->expr.get());
        if (!ok) return;
      } else {
        ty = Ty::VOID;
      }

      if (ty == Ty::UNKNOWN) {
        handler_.Raise(ret->expr->GetPos(), format("unknown ret type"));
        return;
      }
      if (currentFn_->ret != ty) {
        handler_.Raise(
            stmt->GetPos(),
            format("cannot use type %s for function ret type %s",
                   ToString(ty).c_str(), ToString(currentFn_->ret).c_str()));
        return;
      }
    } break;

    case Stmt::Kind::VAR_DECL: {
      auto decl = (VarDeclStmt *)stmt.get();
      if (defTable_.IsDeclaredVar(depth_, decl->name->sval)) {
        handler_.Raise(decl->GetPos(),
                       format("redeclared var %s", decl->name->sval));
        return;
      }
      Ty ty;
      auto ok = InferTy(ty, decl->expr.get());
      if (!ok) return;

      defTable_.InsertVar(std::make_unique<DefVar>(depth_, decl->name->sval,
                                                   decl, decl->isLet, ty));
    } break;

    case Stmt::Kind::ASSIGN: {
      auto assign = (AssignStmt *)stmt.get();
      if (!defTable_.IsDeclaredVar(depth_, assign->name->sval)) {
        handler_.Raise(assign->GetPos(),
                       format("undeclared var %s", assign->name->sval.c_str()));
        return;
      }
      auto def = defTable_.FindVar(depth_, assign->name->sval);
      if (def->isLet) {
        handler_.Raise(assign->GetPos(), format("variable %s is mutable",
                                                assign->name->sval.c_str()));
        return;
      }
      Ty ty;
      bool ok = InferTy(ty, assign->expr.get());
      if (def->ty != ty) {
        handler_.Raise(assign->expr->GetPos(),
                       "assigned expr type doesn't match");
        return;
      }
    } break;

    case Stmt::Kind::IF: {
      auto ifStmt = (IfStmt *)stmt.get();
      Ty condTy;
      auto ok = InferTy(condTy, ifStmt->cond.get());
      if (!ok) return;
      if (condTy != Ty::BOOL) {
        handler_.Raise(ifStmt->cond->GetPos(), "non bool if cond");
        return;
      }

      PushScope();
      Infer(ifStmt->block.get());
      PopScope();

      if (ifStmt->els) {
        if (ifStmt->els->StmtKind() == Stmt::Kind::IF) {
          Infer(ifStmt->els);
        } else if (ifStmt->els->StmtKind() == Stmt::Kind::BLOCK) {
          PushScope();
          Infer(ifStmt->els);
          PopScope();
        }
      }
    } break;
    case Stmt::Kind::BLOCK: {
      auto block = (Block *)stmt.get();
      PushScope();
      Infer(block);
      PopScope();
    } break;
  }
}

bool TyInferer::InferTy(Ty &ty, Expr *expr) {
  switch (expr->ExprKind()) {
    case Expr::Kind::IDENT: {
      auto ident = (Ident *)expr;
      auto def = defTable_.FindVar(depth_, ident->sval);
      if (def == nullptr) {
        handler_.Raise(expr->GetPos(), format("undefined %s", ident->sval));
        return false;
      }
      ty = def->ty;
    } break;
    case Expr::Kind::BINARY: {
      auto binary = (BinaryExpr *)expr;
      Ty lhsTy;
      Ty rhsTy;
      bool ok = InferTy(lhsTy, binary->lhs.get());
      if (!ok) return false;
      ok = InferTy(rhsTy, binary->rhs.get());
      if (!ok) return false;

      if (lhsTy != rhsTy) {
        // TODO: support binary between another types
        handler_.Raise(binary->GetPos(), "another types binary expr");
        return false;
      }

      switch (lhsTy) {
        case Ty::INT:
        case Ty::CHAR:
        case Ty::FLOAT:
          if (binary->op == BinOp::GE || binary->op == BinOp::GT ||
              binary->op == BinOp::LE || binary->op == BinOp::LT) {
            ty = Ty::BOOL;
          } else {
            ty = lhsTy;
          }
          break;
        case Ty::STRING:
        case Ty::BOOL:  // TODO: != ==
        case Ty::VOID:
        case Ty::UNKNOWN:
          handler_.Raise(binary->GetPos(), "unsupported binary expr type");
          return false;
      }
    } break;
    case Expr::Kind::LIT: {
      auto lit = (Lit *)expr;
      switch (lit->LitKind()) {
        case Lit::Kind::INT:
          ty = Ty::INT;
          break;
        case Lit::Kind::FLOAT:
          ty = Ty::FLOAT;
          break;
        case Lit::Kind::BOOL:
          ty = Ty::BOOL;
          break;
        case Lit::Kind::CHAR:
          ty = Ty::CHAR;
          break;
        case Lit::Kind::STR:
          ty = Ty::STRING;
          break;
      }
    } break;
    case Expr::Kind::CALL: {
      auto call = (CallExpr *)expr;
      auto def = defTable_.FindFn(depth_, call->ident->sval);
      if (def == nullptr) {
        handler_.Raise(call->GetPos(), format("undefined function %s",
                                              call->ident->sval.c_str()));
        return false;
      }
      if (def->args.size() != call->args.size()) {
        handler_.Raise(call->GetPos(), "args count doesn't match");
        return false;
      }
      for (int i = 0; i < call->args.size(); i++) {
        auto &arg = call->args[i];
        Ty ty;
        bool ok = InferTy(ty, arg.get());
        if (!ok) return false;
        if (def->args[i] != ty) {
          handler_.Raise(arg->GetPos(), "arg type doesn't match");
          return false;
        }
      }
      ty = def->ret;
    } break;
    case Expr::Kind::UNARY: {
      auto unary = (UnaryExpr *)expr;
      auto unOp = unary->unOp.get();
      Ty exprTy;
      bool ok = InferTy(exprTy, unary->expr.get());
      if (!ok) return false;
      switch (*unOp) {
        case UnOp::NEG:
          if (exprTy == Ty::INT) {
            ty = Ty::INT;
            return true;
          } else if (exprTy == Ty::FLOAT) {
            ty = Ty::FLOAT;
            return true;
          }
          break;
        case UnOp::NOT:
          if (exprTy == Ty::BOOL) {
            ty = Ty::BOOL;
            return true;
          }
          break;
      }
      handler_.Raise(unary->GetPos(),
                     format("invalid unary for expr ty %s", ToString(exprTy)));
      return false;
    } break;
  }
  return true;
}

template <typename... Args>
void TyInferer::Raise(Pos pos, const std::string &fmt, Args... args) {
  handler_.Raise(pos, format(fmt, args...));
}

}  // namespace felis
