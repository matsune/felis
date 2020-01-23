#ifndef PARSER_HPP
#define PARSER_HPP

#include <memory>

using namespace std;

#include "ast.hpp"
#include "lexer.hpp"
#include "source.hpp"
#include "token.hpp"

class Parser {
 private:
  unique_ptr<Lexer> lexer;
  Token peek, peek2;
  Token next();
  unique_ptr<Node> parsePrimary();

 public:
  Parser(unique_ptr<Lexer> lexer, FLEX_STD istream& arg_yyin = cin,
         FLEX_STD ostream& arg_yyout = cout);
  unique_ptr<Node> parse();
  unique_ptr<Node> parseExpr(uint8_t prec = 0);
};

#endif

