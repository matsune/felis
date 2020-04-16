#include "syntax/token.h"

#include "string/string.h"

namespace felis {

namespace {

inline const char *const bool_str(bool b) { return b ? "true" : "false"; }

}  // namespace

void Token::debug() {
  printf("Token {\n");
  printf("\tKind: %s\n", ToString(kind).c_str());
  printf("\tws: %s\n", bool_str(ws));
  printf("\tnl: %s\n", bool_str(nl));
  if (kind == TokenKind::LIT_INT) {
    printf("\tival: %llu\n", ival);
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
}

}  // namespace felis
