#ifndef FELIS_SYNTAX_PARSER_H_
#define FELIS_SYNTAX_PARSER_H_

#include <deque>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "error/handler.h"
#include "syntax/ast.h"
#include "syntax/token.h"

namespace felis {

class Parser {
 public:
  explicit Parser(ErrorHandler &handler) : handler_(handler) {}

  void PushToken(std::unique_ptr<Token> &&token) {
    tokens_.push_back(std::move(token));
  }

  std::unique_ptr<File> Parse();

 private:
  ErrorHandler &handler_;
  std::deque<std::unique_ptr<Token>> tokens_;
  NodeId nextId_ = 1;

  std::unique_ptr<Token> &Peek();
  std::unique_ptr<Token> &Peek2();
  std::unique_ptr<Token> Bump();
  std::unique_ptr<Extern> ParseExtern();
  std::unique_ptr<FnDecl> ParseFnDecl();
  std::unique_ptr<FnProto> ParseFnProto();
  bool ParseFnArgs(std::vector<std::unique_ptr<FnArg>> &args);
  std::unique_ptr<FnArg> ParseFnArg();
  std::unique_ptr<Expr> ParseExpr(uint8_t prec = 0);
  std::unique_ptr<Expr> ParsePrimary();
  std::unique_ptr<Stmt> ParseStmt();
  std::unique_ptr<IfStmt> ParseIfStmt();
  std::unique_ptr<Block> ParseBlock();

  template <typename... Args>
  void Raise(const std::string &fmt, Args... args);
};

}  // namespace felis

#endif  // FELIS_SYNTAX_PARSER_H_
