
#include "string.h"

namespace felis {

std::string ToString(TokenKind kind) {
  switch (kind) {
    case TokenKind::END:
      return "END";
    case TokenKind::IDENT:
      return "IDENT";
    case TokenKind::LIT_INT:
      return "LIT_INT";
    case TokenKind::LIT_FLOAT:
      return "LIT_FLOAT";
    case TokenKind::LIT_BOOL:
      return "LIT_BOOL";
    case TokenKind::LIT_CHAR:
      return "CHAR";
    case TokenKind::LIT_STR:
      return "LIT_STR";
    case TokenKind::PLUS:
      return "+";
    case TokenKind::MINUS:
      return "-";
    case TokenKind::STAR:
      return "*";
    case TokenKind::SLASH:
      return "/";
    case TokenKind::PERCENT:
      return "%";
    case TokenKind::AND:
      return "&";
    case TokenKind::OR:
      return "|";
    case TokenKind::CARET:
      return "^";
    case TokenKind::SHL:
      return "<<";
    case TokenKind::SHR:
      return ">>";
    case TokenKind::ANDAND:
      return "&&";
    case TokenKind::OROR:
      return "||";
    case TokenKind::LT:
      return "<";
    case TokenKind::LE:
      return "<=";
    case TokenKind::GT:
      return ">";
    case TokenKind::GE:
      return ">=";
    case TokenKind::EQEQ:
      return "==";
    case TokenKind::NEQ:
      return "!=";
    case TokenKind::NOT:
      return "!";
    case TokenKind::LPAREN:
      return "(";
    case TokenKind::RPAREN:
      return ")";
    case TokenKind::LBRACE:
      return "{";
    case TokenKind::RBRACE:
      return "}";
    case TokenKind::EQ:
      return "=";
    case TokenKind::SEMI:
      return ";";
    case TokenKind::COLON:
      return ":";
    case TokenKind::COMMA:
      return ",";
    case TokenKind::ARROW:
      return "->";
    default:
      return "unknown";
  }
}

std::string ToString(Ty ty) {
  switch (ty) {
    case Ty::UNKNOWN:
      return "unknown";
    case Ty::VOID:
      return "void";
    case Ty::BOOL:
      return "bool";
    case Ty::INT:
      return "int";
    case Ty::CHAR:
      return "char";
    case Ty::FLOAT:
      return "float";
    case Ty::STRING:
      return "string";
  }
}

std::string ToString(BinOp op) {
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

}  // namespace felis
