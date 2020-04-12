#ifndef FELIS_SYNTAX_PARSER_H_
#define FELIS_SYNTAX_PARSER_H_

#include <deque>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "error/error.h"
#include "syntax/ast.h"
#include "syntax/token.h"

namespace felis {

class Parser {
 public:
  explicit Parser() {}

  void PushToken(std::unique_ptr<Token> &&token) {
    tokens_.push_back(std::move(token));
  }

  std::unique_ptr<File> Parse();

 private:
  std::deque<std::unique_ptr<Token>> tokens_;
  NodeId nextId_ = 1;

  std::unique_ptr<Token> &Peek();
  std::unique_ptr<Token> &Peek2();
  std::unique_ptr<Token> Bump();
  std::unique_ptr<Extern> ParseExtern();
  std::unique_ptr<FnDecl> ParseFnDecl();
  std::unique_ptr<FnProto> ParseFnProto();
  std::vector<std::unique_ptr<FnArg>> ParseFnArgs();
  std::unique_ptr<FnArg> ParseFnArg();
  Expr *ParseExpr(uint8_t prec = 0);
  Expr *ParsePrimary();
  std::unique_ptr<Stmt> ParseStmt();
  std::unique_ptr<IfStmt> ParseIfStmt();
  std::unique_ptr<Block> ParseBlock();

  template <typename... Args>
  void Throw(const std::string &fmt, Args... args);
};

}  // namespace felis

#endif  // FELIS_SYNTAX_PARSER_H_
