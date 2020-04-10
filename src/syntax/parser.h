#ifndef FELIS_SYNTAX_PARSER_H_
#define FELIS_SYNTAX_PARSER_H_

#include <deque>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "err/error.h"
#include "err/result.h"
#include "syntax/ast.h"
#include "syntax/token.h"

namespace felis {

template <class T>
using ParseResult = Result<T, PosError>;

class Parser {
 public:
  explicit Parser() {}

  void PushToken(std::unique_ptr<Token> &&token) {
    tokens_.push_back(std::move(token));
  }

  ParseResult<File> Parse();

 private:
  std::deque<std::unique_ptr<Token>> tokens_;
  NodeId nextId_ = 1;

  std::unique_ptr<Token> &Peek();
  std::unique_ptr<Token> &Peek2();
  std::unique_ptr<Token> Bump();
  ParseResult<Extern> ParseExtern();
  ParseResult<FnDecl> ParseFnDecl();
  ParseResult<FnProto> ParseFnProto();
  ParseResult<FnArgs> ParseFnArgs();
  ParseResult<FnArg> ParseFnArg();
  ParseResult<Expr> ParseExpr(uint8_t prec = 0);
  ParseResult<Expr> ParsePrimary();
  ParseResult<Stmt> ParseStmt();
  ParseResult<IfStmt> ParseIfStmt();
  ParseResult<Block> ParseBlock();

  template <typename T, typename... Args>
  ParseResult<T> Raise(const std::string &fmt, Args... args);
};

}  // namespace felis

#endif  // FELIS_SYNTAX_PARSER_H_
