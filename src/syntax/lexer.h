#ifndef FELIS_SYNTAX_LEXER_H_
#define FELIS_SYNTAX_LEXER_H_

#include <fstream>
#include <iostream>
#include <memory>
#include <string>

#include "error/error.h"
#include "loc.h"
#include "rune.h"
#include "token.h"

namespace felis {

class Lexer {
 public:
  Lexer(std::istream &in) : in_(in), loc_(0), peek_(0) { Consume(); }
  std::unique_ptr<Token> Next();

 private:
  std::istream &in_;
  Loc loc_;
  rune peek_;

  void Consume() {
    try {
      in_ >> peek_;
    } catch (invalid_rune_error &err) {
      Throw(err.what());
    }
  }
  rune Bump();
  bool BumpIf(char);
  bool BumpIf(std::function<bool(rune)>);
  void EatChar(rune &);
  rune Escape();
  void EatString(std::string &);
  void EatLineComment();
  bool EatBlockComment();
  void EatIdent(std::unique_ptr<Token> &);
  void EatNum(std::unique_ptr<Token> &);
  void EatDigits(std::string &, std::function<bool(rune)>);

  template <typename... Args>
  void Throw(const std::string &fmt, Args... args);
};

}  // namespace felis

#endif  // FELIS_SYNTAX_LEXER_H_
