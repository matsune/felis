#ifndef FELIS_SYNTAX_LEXER_H_
#define FELIS_SYNTAX_LEXER_H_

#include <fstream>
#include <iostream>
#include <memory>
#include <string>
#include "syntax/token.h"

namespace felis {

class Lexer {
 public:
  Lexer(std::basic_istream<char> &in, std::string filename = "")
      : in_(in), filename_(filename), error_("") {
    peek_ = Scan();
  }
  std::unique_ptr<Token> Next();

  bool HasError() { return !error_.empty(); }
  std::string Error() { return error_; }

 private:
  std::basic_istream<char> &in_;
  std::string filename_;
  Pos pos_;
  rune peek_;
  std::string error_;

  rune Scan();
  rune Bump();
  bool BumpIf(uint32_t);
  bool BumpIf(std::function<bool(uint32_t)>);
  template <typename... Args>
  void Error(const std::string &fmt, Args... args);
  bool EatChar(rune *);
  bool Escape(char *);
  bool EatString(std::string *);
  void EatLineComment();
  bool EatBlockComment(bool *has_nl);
  bool EatIdent(std::unique_ptr<Token> &);
  bool EatNum(std::unique_ptr<Token> &);
  bool EatDigits(std::string &, std::function<bool(uint32_t)>);
};

}  // namespace felis

#endif  // FELIS_SYNTAX_LEXER_H_
