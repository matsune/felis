#ifndef LEXER_HPP
#define LEXER_HPP

#include <fstream>
#include <iostream>
#include <memory>
#include "token.hpp"

class Lexer {
  basic_istream<char> &in;
  string filename;
  Pos pos;

  rune peek;
  bool eat_ident(unique_ptr<Token> &);
  bool read_digits(string &s, bool f(uint32_t));
  bool eat_decimal_digits(uint64_t &);
  bool eat_num(unique_ptr<Token> &t);
  bool eat_string(string &sval);
  bool eat_char(rune &);
  bool escape(char &c);
  void eatLineComment();
  bool eatBlockComment(bool &);
  rune scan();
  rune getPeek();
  rune bump();
  bool bumpIf(uint32_t);
  bool bumpIf(function<bool(uint32_t)>);
  template <typename... Args>
  bool error(const char *format, Args const &... args);

 public:
  Lexer(basic_istream<char> &in, string filename = "")
      : in(in), filename(filename) {
    peek = scan();
  };
  void setFilename(string filename) { this->filename = filename; };
  bool next(unique_ptr<Token> &t);
};

#endif
