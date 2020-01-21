#ifndef LEXER_HPP
#define LEXER_HPP
#include <vector>
#ifndef FLEX_SCANNER
#include <FlexLexer.h>
#endif
#include "token.hpp"

class Lexer : public yyFlexLexer {
 private:
  bool nl;
  bool ws;
  uint32_t offset;
  vector<uint32_t> lineCols;

  bool makeToken(Token& token, int kind);

 public:
  bool yylex(Token& token);
  Lexer() : nl(false), ws(false), offset(0), lineCols({0}){};
  Pos getPos(uint32_t offset);
};

#endif
