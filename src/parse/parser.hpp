#ifndef PARSER_HPP
#define PARSER_HPP

#include <memory>

using namespace std;

#include "ast.hpp"
#include "common/error.hpp"
/* #include "common/source.hpp" */
#include "lexer.hpp"
#include "token.hpp"

/* class Parser { */
/*  private: */
/*   unique_ptr<Lexer> lexer; */
/*   Token peek, peek2; */
/*   Token next(); */
/*   unique_ptr<Node> parsePrimary(); */
/*   ErrorHandler& handler; */
/*   void error(string msg); */

/*  public: */
/*   Parser(unique_ptr<Lexer>, ErrorHandler&, FLEX_STD istream& arg_yyin = cin,
 */
/*          FLEX_STD ostream& arg_yyout = cout); */
/*   unique_ptr<Node> parse(); */
/*   unique_ptr<Node> parseExpr(uint8_t prec = 0); */
/* }; */

#endif

