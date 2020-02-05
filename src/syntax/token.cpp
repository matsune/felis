#include "syntax.hpp"

string to_string(TokenKind kind) {
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

#define bool_str(b) (b ? "true" : "false")

void Token::debug() {
  printf("Token {\n");
  printf("\tKind: %s\n", to_string(kind).c_str());
  printf("\tws: %s\n", bool_str(ws));
  printf("\tnl: %s\n", bool_str(nl));
  if (kind == TokenKind::LIT_INT) {
    printf("\tival: %lu\n", ival);
  } else if (kind == TokenKind::LIT_FLOAT) {
    printf("\tfval: %lf\n", fval);
  } else if (kind == TokenKind::LIT_BOOL) {
    printf("\tbval: %s\n", bool_str(bval));
  } else if (kind == TokenKind::LIT_STR) {
    printf("\tsval: %s\n", sval.c_str());
  } else if (kind == TokenKind::LIT_CHAR) {
    char buf[4] = {0};
    cval.encode_utf8(buf);
    printf("\tcval: %s\n", buf);
  } else if (kind == TokenKind::IDENT) {
    printf("\tsval: %s\n", sval.c_str());
  }
  printf("}\n");
};
