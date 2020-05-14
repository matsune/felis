#ifndef FELIS_SYNTAX_PARSER_H_
#define FELIS_SYNTAX_PARSER_H_

#include <deque>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "error/error.h"
#include "node/ast.h"
#include "syntax/token.h"

namespace felis {

class Parser {
 public:
  explicit Parser(std::deque<std::unique_ptr<Token>> tokens)
      : tokens_(std::move(tokens)) {}

  std::unique_ptr<ast::File> Parse();

 private:
  std::deque<std::unique_ptr<Token>> tokens_;

  std::unique_ptr<Token> &Peek();
  std::unique_ptr<Token> &Peek2();
  std::unique_ptr<Token> Bump();
  std::unique_ptr<ast::Extern> ParseExtern();
  std::unique_ptr<ast::FnDecl> ParseFnDecl();
  std::unique_ptr<ast::FnProto> ParseFnProto();
  std::unique_ptr<ast::FnArgs> ParseFnArgs();
  std::unique_ptr<ast::FnArg> ParseFnArg();
  std::unique_ptr<ast::Expr> ParseExpr(uint8_t prec = 0);
  std::unique_ptr<ast::Expr> ParsePrimary();
  std::unique_ptr<ast::Stmt> ParseStmt();
  std::unique_ptr<ast::IfStmt> ParseIfStmt();
  std::unique_ptr<ast::Block> ParseBlock();

  template <typename... Args>
  void Throw(const std::string &fmt, Args... args);
};

}  // namespace felis

#endif  // FELIS_SYNTAX_PARSER_H_
