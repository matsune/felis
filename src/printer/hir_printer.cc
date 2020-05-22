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
    PrintPtr(fn);
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
    PrintPtr(block);
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
      PrintExpr(dynamic_cast<hir::Expr *>(stmt));
      break;
    case hir::Stmt::Kind::RET:
      Down("Ret {");
      {
        auto ret = dynamic_cast<hir::RetStmt *>(stmt);
        PrintPtr(ret);
        PrintExpr(ret->expr.get());
      }
      Up("}");
      break;
    case hir::Stmt::Kind::VAR_DECL:
      Down("VarDecl {");
      {
        auto var_decl = dynamic_cast<hir::VarDeclStmt *>(stmt);
        PrintPtr(var_decl);
        Writeln(ToString(*var_decl->decl));
        PrintExpr(var_decl->expr.get());
      }
      Up("}");
      break;
    case hir::Stmt::Kind::ASSIGN:
      Down("Assign {");
      {
        auto assign = dynamic_cast<hir::AssignStmt *>(stmt);
        PrintPtr(assign);
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
        auto binary = dynamic_cast<hir::Binary *>(expr);
        Write("Left: ");
        PrintExpr(binary->lhs.get());
        Writeln("Op: " + ToString(binary->op));
        Write("Right: ");
        PrintExpr(binary->rhs.get());
        PrintPtr(binary);
      }
      Up("}");
      break;

    case hir::Expr::Kind::VALUE: {
      auto value = dynamic_cast<hir::Value *>(expr);
      switch (value->ValueKind()) {
        case hir::Value::Kind::CONSTANT: {
          Down("Value {");
          {
            auto cons = dynamic_cast<hir::Constant *>(value);
            PrintPtr(cons);
            switch (cons->ConstantKind()) {
              case hir::Constant::Kind::INT: {
                auto int_cons = dynamic_cast<hir::IntConstant *>(cons);
                Writeln("CONSTANT %s: %d", ToString(*int_cons->Type()).c_str(),
                        int_cons->val);
              } break;
              case hir::Constant::Kind::FLOAT: {
                auto float_cons = dynamic_cast<hir::FloatConstant *>(cons);
                Writeln("CONSTANT %s: %f",
                        ToString(*float_cons->Type()).c_str(), float_cons->val);
              } break;
              case hir::Constant::Kind::BOOL: {
                auto bool_cons = dynamic_cast<hir::BoolConstant *>(cons);
                const char *v = bool_cons->val ? "true" : "false";
                Writeln("CONSTANT %s: %s", ToString(*bool_cons->Type()).c_str(),
                        v);
              } break;
              case hir::Constant::Kind::STRING: {
                auto str_cons = dynamic_cast<hir::StringConstant *>(cons);
                Writeln("CONSTANT %s: %s", ToString(*str_cons->Type()).c_str(),
                        str_cons->val.c_str());
              } break;
            }
          }
          Up("}");
        } break;
        case hir::Value::Kind::VARIABLE:
          Down("Variable {");
          {
            auto var = dynamic_cast<hir::Variable *>(value);
            PrintPtr(var);
            Writeln(ToString(*var->decl));
          }
          Up("}");
          break;
      }
    } break;

    case hir::Expr::Kind::CALL:
      Down("Call {");
      {
        auto call = dynamic_cast<hir::Call *>(expr);
        PrintPtr(call);
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
        auto unary = dynamic_cast<hir::Unary *>(expr);
        std::string op = unary->op == hir::Unary::Op::NEG ? "-" : "!";
        Writeln("op: %s", op.c_str());
        PrintExpr(unary->expr.get());
        PrintPtr(unary);
      }
      Up("}");
      break;

    case hir::Expr::Kind::IF:
      Down("If {");
      {
        auto if_stmt = dynamic_cast<hir::If *>(expr);
        PrintPtr(if_stmt);
        Write("Cond: ");
        PrintExpr(if_stmt->cond.get());
        PrintBlock(if_stmt->block.get());
        Write("Else: ");
        PrintStmt(if_stmt->els.get());
      }
      Up("}");
      break;
    case hir::Expr::Kind::BLOCK:
      PrintBlock(dynamic_cast<hir::Block *>(expr));
      break;
  }
}

}  // namespace felis
