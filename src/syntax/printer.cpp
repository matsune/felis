#include "syntax.hpp"

#define checkNull(e) \
  if (!e) {          \
    writeln("null"); \
    return;          \
  }

using namespace std;

string binop_string(BinOp op) {
  switch (op) {
    case BinOp::LT:
      return ">";
    case BinOp::LE:
      return ">=";
    case BinOp::GT:
      return "<";
    case BinOp::GE:
      return "<=";
    case BinOp::ADD:
      return "+";
    case BinOp::SUB:
      return "-";
    case BinOp::MUL:
      return "*";
    case BinOp::DIV:
      return "/";
    case BinOp::MOD:
      return "%";
  }
}

void Printer::writeLineNum() { printf("%4d ", line); };

void Printer::indent() {
  for (int i = 0; i < depth; i++) {
    cout << ". ";
  }
};

template <typename... Args>
void Printer::write(const string format, Args const &... args) {
  if (afterNl) {
    indent();
  }
  printf(format.c_str(), args...);
  afterNl = false;
};

template <typename... Args>
void Printer::writeln(const string format, Args const &... args) {
  if (afterNl) {
    indent();
  }
  printf(format.c_str(), args...);
  cout << endl;
  line++;
  writeLineNum();
  afterNl = true;
};

void Printer::down(string s) {
  writeln(s);
  depth++;
};

void Printer::up(string s) {
  depth--;
  writeln(s);
};

void Printer::print(unique_ptr<Node> &node) {
  if (depth == 0) writeLineNum();

  switch (node->nodeKind()) {
    case Node::Kind::STMT:
      printStmt((Stmt *)node.get());
      break;
  }
};

void Printer::printBlock(Block *block) {
  down("Block {");
  {
    for (int i = 0; i < block->stmts.size(); i++) {
      write("[%d] ", i);
      auto &stmt = block->stmts.at(i);
      printStmt(stmt.get());
    }
  }
  up("}");
}

void Printer::printIdent(Ident *ident) {
  checkNull(ident);
  down("Ident {");
  { writeln("Name: " + ident->sval); }
  up("}");
}

void Printer::printStmt(Stmt *stmt) {
  checkNull(stmt);
  switch (stmt->stmtKind()) {
    case Stmt::Kind::EXPR:
      printExpr((Expr *)stmt);
      break;
    case Stmt::Kind::RET:
      down("Ret {");
      {
        auto ret = (RetStmt *)stmt;
        write("Expr: ");
        printExpr(ret->expr.get());
      }
      up("}");
      break;
    case Stmt::Kind::VAR_DECL:
      down("VarDecl {");
      {
        auto varDecl = (VarDeclStmt *)stmt;
        writeln("Decl: %s", varDecl->isLet ? "let" : "var");
        write("Name: ");
        printIdent(varDecl->name.get());
        write("Expr: ");
        printExpr(varDecl->expr.get());
      }
      up("}");
      break;
    case Stmt::Kind::ASSIGN:
      down("Assign {");
      {
        auto assign = (AssignStmt *)stmt;
        write("Name: ");
        printIdent(assign->name.get());
        write("Expr: ");
        printExpr(assign->expr.get());
      }
      up("}");
      break;
    case Stmt::Kind::IF:
      down("If {");
      {
        auto ifStmt = (IfStmt *)stmt;
        write("Cond: ");
        printExpr(ifStmt->cond.get());
        printBlock(ifStmt->block.get());
        write("Else: ");
        printStmt(ifStmt->els.get());
      }
      up("}");
      break;
    case Stmt::Kind::BLOCK:
      printBlock((Block *)stmt);
      break;
    default:
      cout << "unimplemented" << endl;
      exit(1);
  }
}

void Printer::printExpr(Expr *expr) {
  checkNull(expr);
  switch (expr->exprKind()) {
    case Expr::Kind::IDENT:
      printIdent((Ident *)expr);
      break;
    case Expr::Kind::LIT:
      printLit((Lit *)expr);
      break;
    case Expr::Kind::BINARY:
      down("BinaryExpr {");
      {
        auto binary = (BinaryExpr *)expr;
        write("Left: ");
        print(binary->lhs);
        writeln("Op: " + binop_string(binary->op));
        write("Right: ");
        print(binary->rhs);
      }
      up("}");
      break;
  };
}

void Printer::printLit(Lit *lit) {
  checkNull(lit);
  switch (lit->litKind()) {
    case Lit::Kind::INT:
      down("LitInt {");
      {
        auto l = (LitInt *)lit;
        string s = tostring(l->ival);
        writeln("num: " + s);
      }
      up("}");
      break;
    case Lit::Kind::BOOL:
      down("LitBool {");
      {
        auto l = (LitBool *)lit;
        string s = (l->bval ? "true" : "false");
        writeln("literal: " + s);
      }
      up("}");
      break;
    case Lit::Kind::CHAR:
      down("LitChar {");
      {
        auto l = (LitChar *)lit;
        string s{l->cval};
        writeln("literal: '" + s + "'");
      }
      up("}");
      break;
    case Lit::Kind::STR:
      down("LitSTR {");
      {
        auto l = (LitStr *)lit;
        writeln("literal: \"" + l->sval + "\"");
      }
      up("}");
      break;
  }
}
