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

void Printer::Print(const std::unique_ptr<File> &file) {
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

void Printer::PrintExtern(Extern *ext) {
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

void Printer::PrintFnDecl(FnDecl *fn) {
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

void Printer::PrintProto(FnProto *proto) {
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

void Printer::PrintFnArg(FnArg *arg) {
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

void Printer::PrintBlock(Block *block) {
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

void Printer::PrintIdent(Ident *ident) {
  if (!ident) {
    Writeln("null");
    return;
  }

  Down("Ident {");
  {
    Writeln("Name: " + ident->sval);
    PrintPos(ident->GetPos());
  }
  Up("}");
}

void Printer::PrintStmt(Stmt *stmt) {
  if (!stmt) {
    Writeln("null");
    return;
  }

  switch (stmt->StmtKind()) {
    case Stmt::Kind::EXPR:
      PrintExpr(reinterpret_cast<Expr *>(stmt));
      break;
    case Stmt::Kind::RET:
      Down("Ret {");
      {
        auto ret = reinterpret_cast<RetStmt *>(stmt);
        Write("Expr: ");
        PrintExpr(ret->expr.get());
        PrintPos(ret->GetPos());
      }
      Up("}");
      break;
    case Stmt::Kind::VAR_DECL:
      Down("VarDecl {");
      {
        auto varDecl = reinterpret_cast<VarDeclStmt *>(stmt);
        Writeln("Decl: %s", varDecl->isLet ? "let" : "var");
        Write("Name: ");
        PrintIdent(varDecl->name.get());
        Write("Expr: ");
        PrintExpr(varDecl->expr.get());
        PrintPos(varDecl->GetPos());
      }
      Up("}");
      break;
    case Stmt::Kind::ASSIGN:
      Down("Assign {");
      {
        auto assign = reinterpret_cast<AssignStmt *>(stmt);
        Write("Name: ");
        PrintIdent(assign->name.get());
        Write("Expr: ");
        PrintExpr(assign->expr.get());
        PrintPos(assign->GetPos());
      }
      Up("}");
      break;
    case Stmt::Kind::IF:
      Down("If {");
      {
        auto ifStmt = reinterpret_cast<IfStmt *>(stmt);
        Write("Cond: ");
        PrintExpr(ifStmt->cond.get());
        PrintBlock(ifStmt->block.get());
        Write("Else: ");
        PrintStmt(ifStmt->els.get());
        PrintPos(ifStmt->GetPos());
      }
      Up("}");
      break;
    case Stmt::Kind::BLOCK:
      PrintBlock(reinterpret_cast<Block *>(stmt));
      break;
    default:
      std::cout << "unimplemented" << std::endl;
      exit(1);
  }
}

void Printer::PrintExpr(Expr *expr) {
  if (!expr) {
    Writeln("null");
    return;
  }

  switch (expr->ExprKind()) {
    case Expr::Kind::IDENT:
      PrintIdent(reinterpret_cast<Ident *>(expr));
      break;
    case Expr::Kind::LIT:
      PrintLit(reinterpret_cast<Lit *>(expr));
      break;
    case Expr::Kind::BINARY:
      Down("BinaryExpr {");
      {
        auto binary = reinterpret_cast<BinaryExpr *>(expr);
        Write("Left: ");
        PrintExpr(binary->lhs.get());
        Writeln("Op: " + ToString(binary->op));
        Write("Right: ");
        PrintExpr(binary->rhs.get());
        PrintPos(binary->GetPos());
      }
      Up("}");
      break;
    case Expr::Kind::CALL:
      Down("Call {");
      {
        auto call = reinterpret_cast<CallExpr *>(expr);
        Write("Ident: ");
        PrintIdent(call->ident.get());
        Down("Args [");
        {
          for (int i = 0; i < call->args.size(); i++) {
            PrintIndex(i);
            Expr *arg = call->args.at(i).get();
            PrintExpr(arg);
          }
        }
        Up("]");
      }
      Up("}");
      break;
    case Expr::Kind::UNARY:
      Down("Unary {");
      {
        auto unary = reinterpret_cast<UnaryExpr *>(expr);
        std::string op = unary->unOp == UnOp::NEG ? "-" : "!";
        Writeln("op: %s", op.c_str());
        PrintExpr(unary->expr.get());
        PrintPos(unary->GetPos());
      }
      Up("}");
      break;
  }
}

void Printer::PrintLit(Lit *lit) {
  if (!lit) {
    Writeln("null");
    return;
  }

  switch (lit->LitKind()) {
    case Lit::Kind::INT:
      Down("LitInt {");
      {
        auto l = reinterpret_cast<LitInt *>(lit);
        std::string s = tostring(l->ival);
        Writeln("num: " + s);
        PrintPos(lit->GetPos());
      }
      Up("}");
      break;
    case Lit::Kind::BOOL:
      Down("LitBool {");
      {
        auto l = reinterpret_cast<LitBool *>(lit);
        std::string s = (l->bval ? "true" : "false");
        Writeln("literal: " + s);
        PrintPos(lit->GetPos());
      }
      Up("}");
      break;
    case Lit::Kind::CHAR:
      Down("LitChar {");
      {
        auto l = reinterpret_cast<LitChar *>(lit);
        // FIXME
        std::string s{char(l->cval.scalar)};
        Writeln("literal: '" + s + "'");
        PrintPos(lit->GetPos());
      }
      Up("}");
      break;
    case Lit::Kind::STR:
      Down("LitSTR {");
      {
        auto l = reinterpret_cast<LitStr *>(lit);
        Writeln("literal: \"" + l->sval + "\"");
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
