#ifndef LEXER_HPP
#define LEXER_HPP

#include <fstream>
#include <iostream>
#include "common/pos.hpp"
#include "token.hpp"

using namespace std;

struct rune {
  uint32_t val = 0;
  int8_t len_utf8 = 0;
  char bytes[4] = {0};

  bool operator==(const int32_t &c) const { return val == c; };
  bool operator!=(const int32_t &c) const { return val != c; };
  bool operator==(const rune &r) const { return val == r.val; };
};

class Lexer {
  ifstream &in;
  string filename;
  Pos pos;

  rune peek;
  bool eat_ident(Token &);
  bool read_digits(string &s, bool f(uint32_t));
  bool eat_decimal_digits(uint64_t &);
  bool eat_num(Token &t);
  bool eat_string(string &sval);
  bool eat_char(uint64_t &ival);
  bool escape(char &c);
  void eatLineComment();
  bool eatBlockComment(bool &);
  rune scan();
  template <typename... Args>
  bool error(const char *format, Args const &... args);

 public:
  Lexer(ifstream &in, string filename = "") : in(in), filename(filename) {
    peek = scan();
  };
  void setFilename(string filename) { this->filename = filename; };
  rune getPeek();
  rune bump();
  bool next(Token &t);
};

#endif
