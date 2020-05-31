#ifndef FELIS_SYNTAX_PARSER_H_
#define FELIS_SYNTAX_PARSER_H_

#include <deque>
#include <memory>
#include <string>
#include <vector>

#include "error/error.h"
#include "lexer.h"
#include "node/ast.h"
#include "syntax/token.h"
#include "unique.h"

namespace felis {

class Parser {
 public:
  explicit Parser(unique_deque<Token> tokens) : tokens_(std::move(tokens)) {}

  std::unique_ptr<ast::File> Parse();

 private:
  unique_deque<Token> tokens_;
  inline const std::unique_ptr<Token> &Peek() const;
  inline const std::unique_ptr<Token> &Peek2() const;
  inline std::unique_ptr<Token> Bump();
  inline bool Match(Token::Kind);

  std::unique_ptr<ast::Extern> ParseExtern();
  std::unique_ptr<ast::FnDecl> ParseFnDecl();
  std::unique_ptr<ast::FnProto> ParseFnProto();
  std::unique_ptr<ast::FnArgs> ParseFnArgs();
  std::unique_ptr<ast::FnArg> ParseFnArg();
  std::unique_ptr<ast::Type> ParseType();
  std::unique_ptr<ast::Expr> ParseExpr(ast::Block *, uint8_t prec = 0);
  std::unique_ptr<ast::Expr> ParsePrimary(ast::Block *);
  std::unique_ptr<ast::If> ParseIf(ast::Block *);
  std::unique_ptr<ast::Block> ParseBlock(ast::Block *);
  std::unique_ptr<ast::Stmt> ParseStmt(ast::Block *);

  template <typename... Args>
  void Throw(const std::string &fmt, Args... args);
};

std::unique_ptr<felis::ast::File> ParseAst(std::ifstream &);

}  // namespace felis

#endif  // FELIS_SYNTAX_PARSER_H_
