#include "printer/hir_printer.h"

#include <sstream>

#include "string/string.h"

namespace felis {

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
  { Writeln(ToString(*ext->decl)); }
  Up("}");
}

void HirPrinter::PrintFnDecl(hir::FnDecl *fn) {
  Down("FnDecl {");
  {
    Writeln(ToString(*fn->decl));

    Down("FnArgs [");
    {
      for (int i = 0; i < fn->args.size(); i++) {
        PrintIndex(i);
        Writeln(ToString(*fn->args.at(i)));
      }
    }
    Up("]");

    PrintBlock(fn->block.get());
  }
  Up("}");
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
        Writeln(ToString(*var_decl->decl));
        PrintExpr(var_decl->expr.get());
      }
      Up("}");
      break;
    case hir::Stmt::Kind::ASSIGN:
      Down("Assign {");
      {
        auto assign = (hir::AssignStmt *)stmt;
        Writeln(ToString(*assign->decl));
        PrintExpr(assign->expr.get());
      }
      Up("}");
      break;
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
          {
            auto int_cons = dynamic_cast<hir::IntConstant *>(value);
            if (int_cons) {
              Writeln("CONSTANT %s: %d", ToString(*int_cons->Type()).c_str(),
                      int_cons->val);
            }
          }
          {
            auto float_cons = dynamic_cast<hir::FloatConstant *>(value);
            if (float_cons) {
              Writeln("CONSTANT %s: %f", ToString(*float_cons->Type()).c_str(),
                      float_cons->val);
            }
          }
          {
            auto bool_cons = dynamic_cast<hir::BoolConstant *>(value);
            if (bool_cons) {
              Writeln("CONSTANT %s: %s", ToString(*bool_cons->Type()).c_str(),
                      bool_cons->val ? "true" : "false");
            }
          }
          {
            auto str_cons = dynamic_cast<hir::StringConstant *>(value);
            if (str_cons) {
              Writeln("CONSTANT %s: %s", ToString(*str_cons->Type()).c_str(),
                      str_cons->val.c_str());
            }
          }
        } break;
        case hir::Value::Kind::VARIABLE:
          Down("Variable {");
          {
            auto var = (hir::Variable *)value;
            Writeln(ToString(*var->decl));
          }
          Up("}");
          break;
      }
    } break;

    case hir::Expr::Kind::CALL:
      Down("Call {");
      {
        auto call = (hir::Call *)expr;
        Writeln(ToString(*call->decl));
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
