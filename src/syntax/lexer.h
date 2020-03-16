#ifndef FELIS_SYNTAX_LEXER_H_
#define FELIS_SYNTAX_LEXER_H_

#include <fstream>
#include <iostream>
#include <memory>
#include <string>

#include "error/handler.h"
#include "pos.h"
#include "token.h"

namespace felis {

class Lexer {
 public:
  Lexer(std::basic_istream<char> &in, ErrorHandler &handler)
      : in_(in), handler_(handler) {
    peek_ = Scan();
  }
  std::unique_ptr<Token> Next();

 private:
  ErrorHandler &handler_;
  std::basic_istream<char> &in_;
  Pos pos_;
  rune peek_;

  rune Scan();
  rune Bump();
  bool BumpIf(uint32_t);
  bool BumpIf(std::function<bool(uint32_t)>);
  bool EatChar(rune *);
  bool Escape(char *);
  bool EatString(std::string *);
  void EatLineComment();
  bool EatBlockComment(bool *has_nl);
  bool EatIdent(std::unique_ptr<Token> &);
  bool EatNum(std::unique_ptr<Token> &);
  bool EatDigits(std::string &, std::function<bool(uint32_t)>);

  template <typename... Args>
  void Raise(const std::string &fmt, Args... args);
};

}  // namespace felis

#endif  // FELIS_SYNTAX_LEXER_H_
