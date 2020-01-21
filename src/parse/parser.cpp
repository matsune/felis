#include "parser.hpp"

void Parser::next() {
  peek = move(peek2);
  lexer.yylex(peek2);
}

void Parser::parse() {
  while (peek.kind) {
    auto tok = peek;
    cout << "kind " << tok.kind << " nl:" << tok.nl << " ws:" << tok.ws
         << "offset " << tok.offset << "len " << tok.len << endl;
    next();
  };
}
