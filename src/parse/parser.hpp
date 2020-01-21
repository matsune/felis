#ifndef PARSER_HPP
#define PARSER_HPP

#include <memory>

using namespace std;

#include "ast.hpp"
#include "lexer.hpp"
#include "token.hpp"

class Parser {
 private:
  Lexer lexer;
  Token peek, peek2;
  Token next();
  Node* parsePrimary();
  bool isNext(TokenKind kind);

 public:
  Parser(FLEX_STD istream& arg_yyin, FLEX_STD ostream& arg_yyout) {
    lexer.switch_streams(arg_yyin, arg_yyout);
    lexer.yylex(peek);
    lexer.yylex(peek2);
  };
  void parse();
  Node* parseExpr(uint8_t prec = 0);
};

#endif

