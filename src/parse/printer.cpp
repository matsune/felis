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
    case Node::Kind::IDENT:
      down("Ident {");
      {
        auto ident = (Ident *)node.get();
        writeln("Name: " + ident->sval);
      }
      up("}");
      break;
    case Node::Kind::LIT: {
      auto lit = (Lit *)node.get();
      switch (lit->litKind()) {
        case Lit::Kind::INT:
          down("LitInt {");
          {
            auto lit = (LitInt *)node.get();
            string s = tostring(lit->ival);
            writeln("num: " + s);
          }
          up("}");
          break;
        case Lit::Kind::BOOL:
          down("LitBool {");
          {
            auto lit = (LitBool *)node.get();
            string s = (lit->bval ? "true" : "false");
            writeln("literal: " + s);
          }
          up("}");
          break;
        case Lit::Kind::CHAR:
          down("LitChar {");
          {
            auto lit = (LitChar *)node.get();
            string s{lit->cval};
            writeln("literal: '" + s + "'");
          }
          up("}");
          break;
        case Lit::Kind::STR:
          down("LitSTR {");
          {
            auto lit = (LitStr *)node.get();
            writeln("literal: \"" + lit->sval + "\"");
          }
          up("}");
          break;
      }
    } break;
    case Node::Kind::BINARY:
      down("Binary {");
      {
        auto binary = (Binary *)node.get();
        write("Left: ");
        print(binary->lhs);
        writeln("Op: " + binop_string(binary->op));
        write("Right: ");
        print(binary->rhs);
      }
      up("}");
      break;
    default:
      cout << "unimplemented" << endl;
      return;
  }
};

