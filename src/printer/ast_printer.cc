#include "printer/ast_printer.h"

#include <sstream>

#include "string/string.h"

namespace felis {

namespace {

inline std::string bool_str(bool f) { return f ? "true" : "false"; }

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
    PrintLoc(ext);
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
    PrintLoc(fn);
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
      auto arg = proto->args->list.at(i).get();
      PrintFnArg(arg);
    }
  }
  Up("]");
  if (proto->ret) {
    PrintType(proto->ret.get());
  }
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
    PrintType(arg->type_name.get());
    PrintPtr(arg);
    PrintLoc(arg);
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
    Writeln("parent: %p, Result: %d", block->parent, block->StmtResult());
    PrintPtr(block);
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
    Writeln("parent: %p, Result: %d", ident->parent, ident->StmtResult());
    PrintPtr(ident);
    PrintLoc(ident);
  }
  Up("}");
}

void AstPrinter::PrintType(ast::Type *ty) {
  if (!ty) {
    Writeln("null");
    return;
  }

  Down("Type {");
  {
    switch (ty->TypeKind()) {
      case ast::Type::Kind::IDENT: {
        auto ident = dynamic_cast<ast::TypeIdent *>(ty);
        Writeln("Name: " + ident->val);
      } break;
      case ast::Type::Kind::ARRAY: {
        auto array = dynamic_cast<ast::ArrayType *>(ty);
        Write("Elem: ");
        PrintType(array->elem.get());
        Write("Size: ");
        PrintLit(array->size_lit.get());
      } break;
    }
    PrintPtr(ty);
    PrintLoc(ty);
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
      PrintExpr(dynamic_cast<ast::Expr *>(stmt));
      break;
    case ast::Stmt::Kind::RET:
      Down("Ret {");
      {
        auto ret = dynamic_cast<ast::RetStmt *>(stmt);
        Write("Expr: ");
        PrintExpr(ret->expr.get());
        Writeln("parent: %p, Result: %d", ret->parent, ret->StmtResult());
        PrintPtr(ret);
        PrintLoc(ret);
      }
      Up("}");
      break;
    case ast::Stmt::Kind::VAR_DECL:
      Down("VarDecl {");
      {
        auto var_decl = dynamic_cast<ast::VarDeclStmt *>(stmt);
        Writeln("Decl: %s", var_decl->is_let ? "let" : "var");
        Write("Name: ");
        PrintIdent(var_decl->name.get());
        Write("Ty: ");
        PrintType(var_decl->type_name.get());
        Write("Expr: ");
        PrintExpr(var_decl->expr.get());
        Writeln("parent: %p, Result: %d", var_decl->parent,
                var_decl->StmtResult());
        PrintPtr(var_decl);
        PrintLoc(var_decl);
      }
      Up("}");
      break;
    case ast::Stmt::Kind::ASSIGN:
      Down("Assign {");
      {
        auto assign = dynamic_cast<ast::AssignStmt *>(stmt);
        Write("Name: ");
        PrintIdent(assign->name.get());
        Write("Expr: ");
        PrintExpr(assign->expr.get());
        Writeln("parent: %p, Result: %d", assign->parent, assign->StmtResult());
        PrintPtr(assign);
        PrintLoc(assign);
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
      PrintIdent(dynamic_cast<ast::Ident *>(expr));
      break;
    case ast::Expr::Kind::LIT:
      PrintLit(dynamic_cast<ast::Lit *>(expr));
      break;
    case ast::Expr::Kind::BINARY:
      Down("BinaryExpr {");
      {
        auto binary = dynamic_cast<ast::BinaryExpr *>(expr);
        Write("Left: ");
        PrintExpr(binary->lhs.get());
        Writeln("Op: " + ToString(binary->op->op));
        Write("Right: ");
        PrintExpr(binary->rhs.get());
        Writeln("parent: %p, Result: %d", binary->parent, binary->StmtResult());
        PrintPtr(binary);
        PrintLoc(binary);
      }
      Up("}");
      break;
    case ast::Expr::Kind::CALL:
      Down("Call {");
      {
        auto call = dynamic_cast<ast::CallExpr *>(expr);
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
        Writeln("parent: %p, Result: %d", call->parent, call->StmtResult());
        PrintPtr(call);
        PrintLoc(call);
      }
      Up("}");
      break;
    case ast::Expr::Kind::UNARY:
      Down("Unary {");
      {
        auto unary = dynamic_cast<ast::UnaryExpr *>(expr);
        std::string op = unary->op->kind == ast::UnaryOp::NEG ? "-" : "!";
        Writeln("op: %s", op.c_str());
        PrintExpr(unary->expr.get());
        Writeln("parent: %p, Result: %d", unary->parent, unary->StmtResult());
        PrintPtr(unary);
        PrintLoc(unary);
      }
      Up("}");
      break;
    case ast::Expr::Kind::IF:
      Down("If {");
      {
        auto if_stmt = dynamic_cast<ast::If *>(expr);
        Write("Cond: ");
        PrintExpr(if_stmt->cond.get());
        PrintBlock(if_stmt->block.get());
        Write("Else: ");
        PrintExpr(if_stmt->els.get());
        Writeln("parent: %p, Result: %d", if_stmt->parent,
                if_stmt->StmtResult());
        PrintPtr(if_stmt);
        PrintLoc(if_stmt);
      }
      Up("}");
      break;
    case ast::Expr::Kind::BLOCK:
      PrintBlock(dynamic_cast<ast::Block *>(expr));
      break;
    case ast::Expr::Kind::ARRAY:
      Down("Array [");
      {
        auto array = dynamic_cast<ast::ArrayExpr *>(expr);
        for (int i = 0; i < array->exprs.size(); i++) {
          PrintIndex(i);
          auto expr = array->exprs.at(i).get();
          PrintExpr(expr);
        }
        Writeln("parent: %p, Result: %d", array->parent, array->StmtResult());
        PrintPtr(array);
        PrintLoc(array);
      }
      Up("]");
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
        auto l = dynamic_cast<ast::Lit *>(lit);
        Writeln("Int: " + l->val);
        Writeln("parent: %p, Result: %d", l->parent, l->StmtResult());
        PrintPtr(l);
        PrintLoc(l);
      }
      Up("}");
      break;
    case ast::Lit::Kind::FLOAT:
      Down("LitFloat {");
      {
        auto l = dynamic_cast<ast::Lit *>(lit);
        Writeln("Float: " + l->val);
        Writeln("parent: %p, Result: %d", l->parent, l->StmtResult());
        PrintPtr(l);
        PrintLoc(l);
      }
      Up("}");
      break;
    case ast::Lit::Kind::BOOL:
      Down("LitBool {");
      {
        auto l = dynamic_cast<ast::Lit *>(lit);
        Writeln("Bool: " + l->val);
        Writeln("parent: %p, Result: %d", l->parent, l->StmtResult());
        PrintPtr(l);
        PrintLoc(l);
      }
      Up("}");
      break;
    case ast::Lit::Kind::CHAR:
      Down("LitChar {");
      {
        auto l = dynamic_cast<ast::Lit *>(lit);
        Writeln("Char: '" + l->val + "'");
        Writeln("parent: %p, Result: %d", l->parent, l->StmtResult());
        PrintPtr(l);
        PrintLoc(l);
      }
      Up("}");
      break;
    case ast::Lit::Kind::STRING:
      Down("LitSTR {");
      {
        auto l = dynamic_cast<ast::Lit *>(lit);
        Writeln("literal: \"" + l->val + "\"");
        Writeln("parent: %p, Result: %d", l->parent, l->StmtResult());
        PrintPtr(l);
        PrintLoc(l);
      }
      Up("}");
      break;
  }
}

}  // namespace felis
