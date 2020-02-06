#ifndef FELIS_SYNTAX_LEXER_H_
#define FELIS_SYNTAX_LEXER_H_

#include <fstream>
#include <iostream>
#include <memory>
#include "syntax/token.h"

namespace felis {

class Lexer {
 public:
  Lexer(std::basic_istream<char> &in, std::string filename = "")
      : in(in), filename(filename) {
    peek = scan();
  }
  void setFilename(std::string filename) { this->filename = filename; }
  bool next(std::unique_ptr<Token> &t);
  std::basic_istream<char> &in;
  std::string filename;
  Pos pos;

 private:
  rune peek;
  bool eat_ident(std::unique_ptr<Token> &);
  bool read_digits(std::string &s, bool f(uint32_t));
  bool eat_decimal_digits(uint64_t &);
  bool eat_num(std::unique_ptr<Token> &t);
  bool eat_string(std::string &sval);
  bool eat_char(rune &);
  bool escape(char &c);
  void eatLineComment();
  bool eatBlockComment(bool &);
  rune scan();
  rune getPeek() { return peek; }
  rune bump();
  bool bumpIf(uint32_t);
  bool bumpIf(std::function<bool(uint32_t)>);
  template <typename... Args>
  bool error(const char *format, Args const &... args);
};

}  // namespace felis

#endif  // FELIS_SYNTAX_LEXER_H_
