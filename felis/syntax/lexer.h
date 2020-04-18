#ifndef FELIS_SYNTAX_LEXER_H_
#define FELIS_SYNTAX_LEXER_H_

#include <fstream>
#include <iostream>
#include <memory>
#include <string>

#include "error/error.h"
#include "pos.h"
#include "token.h"

namespace felis {

class Lexer {
 public:
  Lexer(std::basic_istream<char> &in) : in_(in) { peek_ = Scan(); }
  std::unique_ptr<Token> Next();

 private:
  std::basic_istream<char> &in_;
  Pos pos_;
  rune peek_;

  rune Scan();
  rune Bump();
  bool BumpIf(uint32_t);
  bool BumpIf(std::function<bool(uint32_t)>);
  rune EatChar();
  char Escape();
  void EatString(std::string &);
  void EatLineComment();
  bool EatBlockComment();
  void EatIdent(Token *);
  void EatNum(Token *);
  void EatDigits(std::string &, std::function<bool(uint32_t)>);

  template <typename... Args>
  void Throw(const std::string &fmt, Args... args);
};

}  // namespace felis

#endif  // FELIS_SYNTAX_LEXER_H_
