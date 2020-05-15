#include "printer/ast_printer.h"

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

void AstPrinter::Print(const std::unique_ptr<ast::File> &file) {
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

void AstPrinter::PrintIndex(int idx) { Write("[%d] ", idx); }

void AstPrinter::PrintExtern(ast::Extern *ext) {
  if (!ext) {
    Writeln("null");
    return;
  }

  Down("Extern {");
  {
    PrintProto(ext->proto.get());
    /* PrintPos(ext->GetPos()); */
  }
  Up("}");
}

void AstPrinter::PrintFnDecl(ast::FnDecl *fn) {
  if (!fn) {
    Writeln("null");
    return;
  }

  Down("FnDecl {");
  {
    PrintProto(fn->proto.get());
    PrintBlock(fn->block.get());
    /* PrintPos(fn->GetPos()); */
  }
  Up("}");
}

void AstPrinter::PrintProto(ast::FnProto *proto) {
  if (!proto) {
    Writeln("null");
    return;
  }

  PrintIdent(proto->name.get());
  Down("FnArgs [");
  {
    for (int i = 0; i < proto->args->list.size(); i++) {
      PrintIndex(i);
      PrintFnArg(proto->args->list.at(i).get());
    }
  }
  Up("]");
}

void AstPrinter::PrintFnArg(ast::FnArg *arg) {
  if (!arg) {
    Writeln("null");
    return;
  }

  Down("FnArg {");
  {
    Write("Name: ");
    PrintIdent(arg->name.get());
    Write("Ty: ");
    PrintIdent(arg->ty.get());
    /* PrintPos(arg->GetPos()); */
  }
  Up("}");
}

void AstPrinter::PrintBlock(ast::Block *block) {
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

void AstPrinter::PrintIdent(ast::Ident *ident) {
  if (!ident) {
    Writeln("null");
    return;
  }

  Down("Ident {");
  {
    Writeln("Name: " + ident->val);
    /* PrintPos(ident->GetPos()); */
  }
  Up("}");
}

void AstPrinter::PrintStmt(ast::Stmt *stmt) {
  if (!stmt) {
    Writeln("null");
    return;
  }

  switch (stmt->StmtKind()) {
    case ast::Stmt::Kind::EXPR:
      PrintExpr((ast::Expr *)stmt);
      break;
    case ast::Stmt::Kind::RET:
      Down("Ret {");
      {
        auto ret = (ast::RetStmt *)stmt;
        Write("Expr: ");
        PrintExpr(ret->expr.get());
        /* PrintPos(ret->GetPos()); */
      }
      Up("}");
      break;
    case ast::Stmt::Kind::VAR_DECL:
      Down("VarDecl {");
      {
        auto var_decl = (ast::VarDeclStmt *)stmt;
        Writeln("Decl: %s", var_decl->is_let ? "let" : "var");
        Write("Name: ");
        PrintIdent(var_decl->name.get());
        Write("Expr: ");
        PrintExpr(var_decl->expr.get());
        /* PrintPos(varDecl->GetPos()); */
      }
      Up("}");
      break;
    case ast::Stmt::Kind::ASSIGN:
      Down("Assign {");
      {
        auto assign = (ast::AssignStmt *)stmt;
        Write("Name: ");
        PrintIdent(assign->name.get());
        Write("Expr: ");
        PrintExpr(assign->expr.get());
        /* PrintPos(assign->GetPos()); */
      }
      Up("}");
      break;
    default:
      std::cout << "unimplemented" << std::endl;
      exit(1);
  }
}

void AstPrinter::PrintExpr(ast::Expr *expr) {
  if (!expr) {
    Writeln("null");
    return;
  }

  switch (expr->ExprKind()) {
    case ast::Expr::Kind::IDENT:
      PrintIdent((ast::Ident *)expr);
      break;
    case ast::Expr::Kind::LIT:
      PrintLit((ast::Lit *)expr);
      break;
    case ast::Expr::Kind::BINARY:
      Down("BinaryExpr {");
      {
        auto binary = (ast::BinaryExpr *)expr;
        Write("Left: ");
        PrintExpr(binary->lhs.get());
        Writeln("Op: " + ToString(binary->op->op));
        Write("Right: ");
        PrintExpr(binary->rhs.get());
        /* PrintPos(binary->GetPos()); */
      }
      Up("}");
      break;
    case ast::Expr::Kind::CALL:
      Down("Call {");
      {
        auto call = (ast::CallExpr *)expr;
        Write("Ident: ");
        PrintIdent(call->ident.get());
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
    case ast::Expr::Kind::UNARY:
      Down("Unary {");
      {
        auto unary = (ast::UnaryExpr *)expr;
        std::string op = unary->op->op == ast::UnaryOp::Op::NEG ? "-" : "!";
        Writeln("op: %s", op.c_str());
        PrintExpr(unary->expr.get());
        /* PrintPos(unary->GetPos()); */
      }
      Up("}");
      break;
    case ast::Expr::Kind::IF:
      Down("If {");
      {
        auto if_stmt = (ast::If *)expr;
        Write("Cond: ");
        PrintExpr(if_stmt->cond.get());
        PrintBlock(if_stmt->block.get());
        Write("Else: ");
        PrintExpr(if_stmt->els.get());
        /* PrintPos(ifStmt->GetPos()); */
      }
      Up("}");
      break;
    case ast::Expr::Kind::BLOCK:
      PrintBlock((ast::Block *)expr);
      break;
  }
}

void AstPrinter::PrintLit(ast::Lit *lit) {
  if (!lit) {
    Writeln("null");
    return;
  }

  switch (lit->LitKind()) {
    case ast::Lit::Kind::INT:
      Down("LitInt {");
      {
        auto l = (ast::Lit *)lit;
        Writeln("Int: " + l->val);
        /* PrintPos(lit->GetPos()); */
      }
      Up("}");
      break;
    case ast::Lit::Kind::FLOAT:
      Down("LitFloat {");
      {
        auto f = (ast::Lit *)lit;
        Writeln("Float: " + f->val);
        /* PrintPos(lit->GetPos()); */
      }
      Up("}");
      break;
    case ast::Lit::Kind::BOOL:
      Down("LitBool {");
      {
        auto l = (ast::Lit *)lit;
        Writeln("Bool: " + l->val);
        /* PrintPos(lit->GetPos()); */
      }
      Up("}");
      break;
    case ast::Lit::Kind::CHAR:
      Down("LitChar {");
      {
        auto l = (ast::Lit *)lit;
        Writeln("Char: '" + l->val + "'");
        /* PrintPos(lit->GetPos()); */
      }
      Up("}");
      break;
    case ast::Lit::Kind::STRING:
      Down("LitSTR {");
      {
        auto l = (ast::Lit *)lit;
        Writeln("literal: \"" + l->val + "\"");
        /* PrintPos(lit->GetPos()); */
      }
      Up("}");
      break;
  }
}

}  // namespace felis
