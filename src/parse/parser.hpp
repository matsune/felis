#include "lexer.hpp"
#include "token.hpp"

class Parser {
 private:
  Lexer lexer;
  Token peek, peek2;
  void next();

 public:
  Parser(FLEX_STD istream& arg_yyin, FLEX_STD ostream& arg_yyout) {
    lexer.switch_streams(arg_yyin, arg_yyout);
    lexer.yylex(peek);
    lexer.yylex(peek2);
  };
  void parse();
};
