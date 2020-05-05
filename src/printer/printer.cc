#include "printer.h"

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

void Printer::Print(const std::unique_ptr<ast::File> &file) {
  WriteLineNum();
  for (int i = 0; i < file->externs.size(); i++) {
    PrintIndex(i);
    PrintExtern(file->externs.at(i).get());
  }
  for (int i = 0; i < file->fnDecls.size(); i++) {
    PrintIndex(i);
    PrintFnDecl(file->fnDecls.at(i).get());
  }
  printf("\n");
}

void Printer::WriteLineNum() { printf("%4d ", line_); }

void Printer::Indent() {
  for (int i = 0; i < depth_; i++) {
    std::cout << ". ";
  }
}

template <typename... Args>
void Printer::Write(const std::string format, Args const &... args) {
  if (after_nl_) {
    Indent();
  }
  printf(format.c_str(), args...);
  after_nl_ = false;
}

template <typename... Args>
void Printer::Writeln(const std::string format, Args const &... args) {
  if (after_nl_) {
    Indent();
  }
  printf(format.c_str(), args...);
  std::cout << std::endl;
  line_++;
  WriteLineNum();
  after_nl_ = true;
}

void Printer::Down(std::string s) {
  Writeln(s);
  depth_++;
}

void Printer::Up(std::string s) {
  depth_--;
  Writeln(s);
}

void Printer::PrintIndex(int idx) { Write("[%d] ", idx); }

void Printer::PrintExtern(ast::Extern *ext) {
  if (!ext) {
    Writeln("null");
    return;
  }

  Down("Extern {");
  {
    PrintProto(ext->proto.get());
    PrintPos(ext->GetPos());
  }
  Up("}");
}

void Printer::PrintFnDecl(ast::FnDecl *fn) {
  if (!fn) {
    Writeln("null");
    return;
  }

  Down("FnDecl {");
  {
    PrintProto(fn->proto.get());
    PrintBlock(fn->block.get());
    PrintPos(fn->GetPos());
  }
  Up("}");
}

void Printer::PrintProto(ast::FnProto *proto) {
  if (!proto) {
    Writeln("null");
    return;
  }

  PrintIdent(proto->name.get());
  Down("FnArgs [");
  {
    for (int i = 0; i < proto->args.size(); i++) {
      PrintIndex(i);
      PrintFnArg(proto->args.at(i).get());
    }
  }
  Up("]");
}

void Printer::PrintFnArg(ast::FnArg *arg) {
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
    PrintPos(arg->GetPos());
  }
  Up("}");
}

void Printer::PrintBlock(ast::Block *block) {
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

void Printer::PrintIdent(ast::Ident *ident) {
  if (!ident) {
    Writeln("null");
    return;
  }

  Down("Ident {");
  {
    Writeln("Name: " + ident->val);
    PrintPos(ident->GetPos());
  }
  Up("}");
}

void Printer::PrintStmt(ast::Stmt *stmt) {
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
        PrintPos(ret->GetPos());
      }
      Up("}");
      break;
    case ast::Stmt::Kind::VAR_DECL:
      Down("VarDecl {");
      {
        auto varDecl = dynamic_cast<ast::VarDeclStmt *>(stmt);
        Writeln("Decl: %s", varDecl->isLet ? "let" : "var");
        Write("Name: ");
        PrintIdent(varDecl->name.get());
        Write("Expr: ");
        PrintExpr(varDecl->expr.get());
        PrintPos(varDecl->GetPos());
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
        PrintPos(assign->GetPos());
      }
      Up("}");
      break;
    case ast::Stmt::Kind::IF:
      Down("If {");
      {
        auto ifStmt = dynamic_cast<ast::IfStmt *>(stmt);
        Write("Cond: ");
        PrintExpr(ifStmt->cond.get());
        PrintBlock(ifStmt->block.get());
        Write("Else: ");
        PrintStmt(ifStmt->els.get());
        PrintPos(ifStmt->GetPos());
      }
      Up("}");
      break;
    case ast::Stmt::Kind::BLOCK:
      PrintBlock(dynamic_cast<ast::Block *>(stmt));
      break;
    default:
      std::cout << "unimplemented" << std::endl;
      exit(1);
  }
}

void Printer::PrintExpr(ast::Expr *expr) {
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
        Writeln("Op: " + ToString(binary->op));
        Write("Right: ");
        PrintExpr(binary->rhs.get());
        PrintPos(binary->GetPos());
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
      }
      Up("}");
      break;
    case ast::Expr::Kind::UNARY:
      Down("Unary {");
      {
        auto unary = dynamic_cast<ast::UnaryExpr *>(expr);
        std::string op = unary->unOp == ast::UnOp::NEG ? "-" : "!";
        Writeln("op: %s", op.c_str());
        PrintExpr(unary->expr.get());
        PrintPos(unary->GetPos());
      }
      Up("}");
      break;
  }
}

void Printer::PrintLit(ast::Lit *lit) {
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
        PrintPos(lit->GetPos());
      }
      Up("}");
      break;
    case ast::Lit::Kind::FLOAT:
      Down("LitFloat {");
      {
        auto f = dynamic_cast<ast::Lit *>(lit);
        Writeln("Float: " + f->val);
        PrintPos(lit->GetPos());
      }
      Up("}");
      break;
    case ast::Lit::Kind::BOOL:
      Down("LitBool {");
      {
        auto l = dynamic_cast<ast::Lit *>(lit);
        Writeln("Bool: " + l->val);
        PrintPos(lit->GetPos());
      }
      Up("}");
      break;
    case ast::Lit::Kind::CHAR:
      Down("LitChar {");
      {
        auto l = dynamic_cast<ast::Lit *>(lit);
        Writeln("Char: '" + l->val + "'");
        PrintPos(lit->GetPos());
      }
      Up("}");
      break;
    case ast::Lit::Kind::STRING:
      Down("LitSTR {");
      {
        auto l = dynamic_cast<ast::Lit *>(lit);
        Writeln("literal: \"" + l->val + "\"");
        PrintPos(lit->GetPos());
      }
      Up("}");
      break;
  }
}

void Printer::PrintPos(Pos pos) {
  Writeln("Pos: line %d, col %d", pos.line, pos.column);
}

}  // namespace felis
