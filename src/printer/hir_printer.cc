#include "printer/hir_printer.h"

#include <sstream>

#include "string/string.h"

namespace felis {

namespace {

template <typename T>
static std::string tostring(const T &t) {
  std::ostringstream ss;
  ss << t;
  return ss.str();
}

}  // namespace

void HirPrinter::Print(const std::unique_ptr<hir::File> &file) {
  WriteLineNum();
  for (int i = 0; i < file->externs.size(); i++) {
    PrintIndex(i);
    PrintExtern(file->externs.at(i).get());
  }
  for (int i = 0; i < file->fn_decls.size(); i++) {
    PrintIndex(i);
    PrintFnDecl(file->fn_decls.at(i).get());
  }
  printf("\n");
}

void HirPrinter::PrintIndex(int idx) { Write("[%d] ", idx); }

void HirPrinter::PrintExtern(hir::Extern *ext) {
  Down("Extern {");
  { PrintDecl(ext->decl.get()); }
  Up("}");
}

void HirPrinter::PrintFnDecl(hir::FnDecl *fn) {
  Down("FnDecl {");
  {
    PrintDecl(fn->decl.get());

    Down("FnArgs [");
    {
      for (int i = 0; i < fn->args.size(); i++) {
        PrintIndex(i);
        PrintDecl(fn->args.at(i).get());
      }
    }
    Up("]");

    PrintBlock(fn->block.get());
  }
  Up("}");
}

void HirPrinter::PrintDecl(Decl *decl) {
  Writeln("%s %s: %s", ToString(decl->kind).c_str(), decl->name.c_str(),
          ToString(decl->type.get()).c_str());
}

void HirPrinter::PrintBlock(hir::Block *block) {
  if (!block) {
    Writeln("null");
    return;
  }

  Down("Block {");
  {
    for (int i = 0; i < block->stmts.size(); i++) {
      PrintIndex(i);
      auto &stmt = block->stmts.at(i);
      PrintStmt(stmt.get());
    }
  }
  Up("}");
}

void HirPrinter::PrintStmt(hir::Stmt *stmt) {
  if (!stmt) {
    Writeln("null");
    return;
  }

  switch (stmt->StmtKind()) {
    case hir::Stmt::Kind::EXPR:
      PrintExpr((hir::Expr *)stmt);
      break;
    case hir::Stmt::Kind::RET:
      Down("Ret {");
      {
        auto ret = (hir::RetStmt *)stmt;
        PrintExpr(ret->expr.get());
      }
      Up("}");
      break;
    case hir::Stmt::Kind::VAR_DECL:
      Down("VarDecl {");
      {
        auto var_decl = (hir::VarDeclStmt *)stmt;
        PrintDecl(var_decl->decl.get());
        PrintExpr(var_decl->expr.get());
      }
      Up("}");
      break;
    case hir::Stmt::Kind::ASSIGN:
      Down("Assign {");
      {
        auto assign = (hir::AssignStmt *)stmt;
        PrintDecl(assign->decl.get());
        PrintExpr(assign->expr.get());
      }
      Up("}");
      break;
    default:
      std::cout << "unimplemented" << std::endl;
      exit(1);
  }
}

void HirPrinter::PrintConstant(hir::Constant *cons) {
  switch (cons->ConstantKind()) {
    case hir::Constant::Kind::INT: {
      auto v = (hir::IntConstant *)cons;
      Writeln("CONSTANT %s: %d", ToString(v->Ty().get()).c_str(), v->val);
    } break;
    case hir::Constant::Kind::FLOAT: {
      auto v = (hir::FloatConstant *)cons;
      Writeln("CONSTANT %s: %f", ToString(v->Ty().get()).c_str(), v->val);
    } break;
    case hir::Constant::Kind::CHAR: {
      auto v = (hir::CharConstant *)cons;
      std::string s = "'";
      s.append(v->val);
      Writeln("CONSTANT %s: %s'", ToString(v->Ty().get()).c_str(), s.c_str());
    } break;
    case hir::Constant::Kind::BOOL: {
      auto v = (hir::BoolConstant *)cons;
      Writeln("CONSTANT %s: %s", ToString(v->Ty().get()).c_str(),
              (v->val ? "true" : "false"));
    } break;
    case hir::Constant::Kind::STRING: {
      auto v = (hir::StringConstant *)cons;
      Writeln("CONSTANT %s: \"%s\"", ToString(v->Ty().get()).c_str(),
              v->val.c_str());
    } break;
  }
}

void HirPrinter::PrintExpr(hir::Expr *expr) {
  if (!expr) {
    Writeln("null");
    return;
  }

  switch (expr->ExprKind()) {
    case hir::Expr::Kind::BINARY:
      Down("BinaryExpr {");
      {
        auto binary = (hir::Binary *)expr;
        Write("Left: ");
        PrintExpr(binary->lhs.get());
        Writeln("Op: " + ToString(binary->op));
        Write("Right: ");
        PrintExpr(binary->rhs.get());
      }
      Up("}");
      break;

    case hir::Expr::Kind::VALUE: {
      auto value = (hir::Value *)expr;
      switch (value->ValueKind()) {
        case hir::Value::Kind::CONSTANT: {
          auto cons = (hir::Constant *)value;
          PrintConstant(cons);
        } break;
        case hir::Value::Kind::VARIABLE:
          Down("Variable {");
          {
            auto var = (hir::Variable *)value;
            PrintDecl(var->decl.get());
          }
          Up("}");
          break;
      }
    } break;

    case hir::Expr::Kind::CALL:
      Down("Call {");
      {
        auto call = (hir::Call *)expr;
        PrintDecl(call->decl.get());
        Down("Args [");
        {
          for (int i = 0; i < call->args.size(); i++) {
            PrintIndex(i);
            auto &arg = call->args.at(i);
            PrintExpr(arg.get());
          }
        }
        Up("]");
      }
      Up("}");
      break;

    case hir::Expr::Kind::UNARY:
      Down("Unary {");
      {
        auto unary = (hir::Unary *)expr;
        std::string op = unary->op == hir::Unary::Op::NEG ? "-" : "!";
        Writeln("op: %s", op.c_str());
        PrintExpr(unary->expr.get());
      }
      Up("}");
      break;

    case hir::Expr::Kind::IF:
      Down("If {");
      {
        auto if_stmt = (hir::If *)expr;
        Write("Cond: ");
        PrintExpr(if_stmt->cond.get());
        PrintBlock(if_stmt->block.get());
        Write("Else: ");
        PrintStmt(if_stmt->els.get());
      }
      Up("}");
      break;
    case hir::Expr::Kind::BLOCK:
      PrintBlock((hir::Block *)expr);
      break;
  }
}

}  // namespace felis
