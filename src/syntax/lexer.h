#ifndef FELIS_SYNTAX_LEXER_H_
#define FELIS_SYNTAX_LEXER_H_

#include <fstream>
#include <iostream>
#include <memory>
#include <string>

#include "err/handler.h"
#include "err/result.h"
#include "pos.h"
#include "token.h"

namespace felis {

template <class T>
using LexResult = Result<T, PosError>;

class Lexer {
 public:
  Lexer(std::basic_istream<char> &in) : in_(in) { peek_ = Scan(); }
  LexResult<Token> Next();

 private:
  std::basic_istream<char> &in_;
  Pos pos_;
  rune peek_;

  rune Scan();
  rune Bump();
  bool BumpIf(uint32_t);
  bool BumpIf(std::function<bool(uint32_t)>);
  LexResult<rune> EatChar();
  LexResult<char> Escape();
  LexResult<std::string> EatString();
  void EatLineComment();
  LexResult<bool> EatBlockComment();
  LexResult<Token> EatIdent();
  LexResult<Token> EatNum();
  LexResult<std::string> EatDigits(std::function<bool(uint32_t)>);

  template <typename T, typename... Args>
  LexResult<T> Raise(const std::string &fmt, Args... args);
};

}  // namespace felis

#endif  // FELIS_SYNTAX_LEXER_H_
