#include "parse.hpp"

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

void Printer::writeln(string msg) {
  if (afterNl) {
    indent();
  }
  cout << msg << endl;
  line++;
  writeLineNum();
  afterNl = true;
};

void Printer::write(string msg) {
  if (afterNl) {
    indent();
  }
  cout << msg;
  afterNl = false;
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

void Printer::printIdent(Ident *ident) {
  down("Ident {");
  { writeln("Name: " + ident->sval); }
  up("}");
}

void Printer::printStmt(Stmt *stmt) {
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
  }
}

void Printer::printExpr(Expr *expr) {
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
