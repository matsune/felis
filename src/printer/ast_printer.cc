#include "printer/ast_printer.h"

#include <sstream>

#include "macro.h"
#include "string/string.h"

namespace felis {

#define NULL_CHECK(ptr) \
  if (!ptr) {           \
    Writeln("null");    \
    return;             \
  }

namespace {

inline std::string bool_str(bool f) { return f ? "true" : "false"; }

}  // namespace

void AstPrinter::Print(const std::unique_ptr<ast::File> &file) {
  WriteLineNum();
  for (int i = 0; i < file->externs.size(); i++) {
    PrintArrayIndex(i);
    PrintExtern(file->externs.at(i));
  }
  for (int i = 0; i < file->funcs.size(); i++) {
    PrintArrayIndex(i);
    PrintFunc(file->funcs.at(i));
  }
  printf("\n");
}

void AstPrinter::PrintArrayIndex(int idx) { Write("[%d] ", idx); }

void AstPrinter::PrintExtern(ast::Extern *ext) {
  NULL_CHECK(ext)

  Down("Extern {");
  {
    PrintProto(ext->proto);
    // PrintLoc(ext);
  }
  Up("}");
}

void AstPrinter::PrintFunc(ast::Func *fn) {
  NULL_CHECK(fn)

  Down("Func {");
  {
    PrintProto(fn->proto);
    PrintBlock(fn->block);
    // PrintLoc(fn);
  }
  Up("}");
}

void AstPrinter::PrintProto(ast::FnProto *proto) {
  NULL_CHECK(proto)

  PrintIdent(proto->name);
  Down("FnArgs [");
  {
    for (int i = 0; i < proto->args->list.size(); i++) {
      PrintArrayIndex(i);
      PrintFnArg(proto->args->list.at(i));
    }
  }
  Up("]");
  PrintTypeName(proto->ret);
}

void AstPrinter::PrintFnArg(ast::FnArg *arg) {
  NULL_CHECK(arg);

  Down("FnArg {");
  {
    Write("Name: ");
    PrintIdent(arg->name);
    Write("Type: ");
    PrintTypeName(arg->type);
    PrintPtr(arg);
    // PrintLoc(arg);
  }
  Up("}");
}

void AstPrinter::PrintBlock(ast::Block *block) {
  NULL_CHECK(block);

  Down("Block {");
  {
    for (int i = 0; i < block->stmts.size(); i++) {
      PrintArrayIndex(i);
      PrintStmt(block->stmts.at(i));
    }
    PrintPtr(block);
  }
  Up("}");
}

void AstPrinter::PrintIdent(ast::Ident *ident) {
  NULL_CHECK(ident);

  Down("Ident {");
  {
    Writeln("Name: " + ident->val);
    PrintPtr(ident);
    // PrintLoc(ident);
  }
  Up("}");
}

void AstPrinter::PrintTypeName(ast::AstNode *ty) {
  NULL_CHECK(ty);

  Down("Type {");
  {
    if (auto ident = node_cast_ornull<ast::Ident>(ty)) {
      Writeln("Name: " + ident->val);
    } else if (auto array = node_cast_ornull<ast::ArrayType>(ty)) {
      Write("Elem: ");
      PrintTypeName(array->elem);
      Write("Size: ");
      PrintLit(array->size_lit);
    } else {
      UNREACHABLE
    }
    PrintPtr(ty);
    // PrintLoc(ty);
  }
  Up("}");
}

void AstPrinter::PrintRet(ast::RetStmt *stmt) {
  Down("Ret {");
  {
    Write("Expr: ");
    PrintExpr(stmt->expr);
    PrintPtr(stmt);
    // PrintLoc(stmt);
  }
  Up("}");
}

void AstPrinter::PrintVarDecl(ast::VarDeclStmt *stmt) {
  Down("VarDecl {");
  {
    Writeln("Decl: %s", stmt->is_let ? "let" : "var");
    Write("Name: ");
    PrintIdent(stmt->name);
    Write("Ty: ");
    PrintTypeName(stmt->type);
    Write("Expr: ");
    PrintExpr(stmt->expr);
    PrintPtr(stmt);
    // PrintLoc(stmt);
  }
  Up("}");
}

void AstPrinter::PrintAssign(ast::AssignStmt *stmt) {
  Down("Assign {");
  {
    Write("Left: ");
    PrintExpr(stmt->left);
    Write("Expr: ");
    PrintExpr(stmt->expr);
    PrintPtr(stmt);
    // PrintLoc(stmt);
  }
  Up("}");
}

void AstPrinter::PrintStmt(ast::AstNode *stmt) {
  NULL_CHECK(stmt)

  if (auto ret_stmt = node_cast_ornull<ast::RetStmt>(stmt)) {
    PrintRet(ret_stmt);
  } else if (auto var_decl_stmt = node_cast_ornull<ast::VarDeclStmt>(stmt)) {
    PrintVarDecl(var_decl_stmt);
  } else if (auto assign_stmt = node_cast_ornull<ast::AssignStmt>(stmt)) {
    PrintAssign(assign_stmt);
  } else {
    PrintExpr(stmt);
  }
}

void AstPrinter::PrintExpr(ast::AstNode *expr) {
  NULL_CHECK(expr)

  if (auto ident = node_cast_ornull<ast::Ident>(expr)) {
    PrintIdent(ident);
  } else if (auto lit = node_cast_ornull<ast::Literal>(expr)) {
    PrintLit(lit);
  } else if (auto binary = node_cast_ornull<ast::Binary>(expr)) {
    PrintBinary(binary);
  } else if (auto call = node_cast_ornull<ast::Call>(expr)) {
    PrintCall(call);
  } else if (auto unary = node_cast_ornull<ast::Unary>(expr)) {
    PrintUnary(unary);
  } else if (auto if_stmt = node_cast_ornull<ast::If>(expr)) {
    PrintIf(if_stmt);
  } else if (auto block = node_cast_ornull<ast::Block>(expr)) {
    PrintBlock(block);
  } else if (auto array = node_cast_ornull<ast::Array>(expr)) {
    PrintArray(array);
  } else if (auto index = node_cast_ornull<ast::Index>(expr)) {
    PrintIndex(index);
  } else {
    UNREACHABLE
  }
}

void AstPrinter::PrintLit(ast::Literal *lit) {
  NULL_CHECK(lit)

  switch (lit->kind) {
    case ast::Literal::Kind::INT:
      Down("LitInt {");
      Writeln("Int: " + lit->val);
      break;
    case ast::Literal::Kind::FLOAT:
      Down("LitFloat {");
      Writeln("Float: " + lit->val);

      break;
    case ast::Literal::Kind::BOOL:
      Down("LitBool {");
      Writeln("Bool: " + lit->val);
      break;
    case ast::Literal::Kind::CHAR:
      Down("LitChar {");
      char c[4];
      lit->r.encode(c);
      Writeln("Char: '%s'", c);
      break;
    case ast::Literal::Kind::STRING:
      Down("LitString {");
      Writeln("String: \"" + lit->val + "\"");
      break;
  }
  PrintPtr(lit);
  // PrintLoc(lit);
  Up("}");
}

void AstPrinter::PrintBinary(ast::Binary *binary) {
  Down("Binary {");
  {
    Write("Left: ");
    PrintExpr(binary->lhs);
    Writeln("Op: " + ToString(binary->op->kind));
    Write("Right: ");
    PrintExpr(binary->rhs);
    PrintPtr(binary);
    // PrintLoc(binary);
  }
  Up("}");
}

void AstPrinter::PrintCall(ast::Call *call) {
  Down("Call {");
  {
    Write("Ident: ");
    PrintIdent(call->ident);
    Down("Args [");
    {
      for (int i = 0; i < call->args.size(); i++) {
        PrintArrayIndex(i);
        PrintExpr(call->args.at(i));
      }
    }
    Up("]");
    PrintPtr(call);
    // PrintLoc(call);
  }
  Up("}");
}

void AstPrinter::PrintUnary(ast::Unary *unary) {
  Down("Unary {");
  {
    std::string op = unary->op->kind == ast::UnaryOp::NEG ? "-" : "!";
    Writeln("op: %s", op.c_str());
    PrintExpr(unary->expr);
    PrintPtr(unary);
    // PrintLoc(unary);
  }
  Up("}");
}

void AstPrinter::PrintIf(ast::If *if_stmt) {
  Down("If {");
  {
    Write("Cond: ");
    PrintExpr(if_stmt->cond);
    PrintBlock(if_stmt->block);
    Write("Else: ");
    PrintExpr(if_stmt->els);
    PrintPtr(if_stmt);
    // PrintLoc(if_stmt);
  }
  Up("}");
}

void AstPrinter::PrintArray(ast::Array *array) {
  Down("Array [");
  {
    for (int i = 0; i < array->exprs.size(); i++) {
      PrintArrayIndex(i);
      PrintExpr(array->exprs.at(i));
    }
    PrintPtr(array);
    // PrintLoc(array);
  }
  Up("]");
}

void AstPrinter::PrintIndex(ast::Index *index) {
  Down("Index {");
  {
    Write("Expr: ");
    PrintExpr(index->expr);
    Write("IdxExpr: ");
    PrintExpr(index->idx_expr);
    PrintPtr(index);
    // PrintLoc(index);
  }
  Up("}");
}

}  // namespace felis
