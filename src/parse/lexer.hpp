#ifndef LEXER_HPP
#define LEXER_HPP
#include <vector>
#ifndef FLEX_SCANNER
#include <FlexLexer.h>
#endif
#include "common/source.hpp"
#include "token.hpp"

class Lexer : public yyFlexLexer {
 private:
  bool nl;
  bool ws;
  uint32_t offset;
  Source& src;

  bool makeToken(Token& token, TokenKind kind);

 public:
  bool yylex(Token& token);
  Lexer(Source& src) : src(src), nl(false), ws(false), offset(0){};
  Pos getPos(uint32_t offset) { return src.getPos(offset); };
};

#endif
