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

  inline const std::unique_ptr<Token>& Peek() const { return tokens_.front(); }

  inline const std::unique_ptr<Token>& Peek2() const { return tokens_[1]; }

  inline std::unique_ptr<Token> Bump() { return tokens_.move_front(); }

  // Test next token
  inline bool Match(Token::Kind kind) { return Peek()->kind == kind; }

  // Throw error if next token doesn't match
  inline std::unique_ptr<Token> Must(Token::Kind kind) {
    if (!Match(kind)) {
      Throw("expected %s (got %s)", ToString(kind).c_str(),
            ToString(Peek()->kind).c_str());
    }
    return Bump();
  }

  ast::Extern* Extern();
  ast::Func* Func();
  ast::FnProto* FnProto();
  ast::FnArgs* FnArgs();
  ast::FnArg* FnArg();
  // Parse type name
  // Returns Ident or ArrayType
  ast::AstNode* TypeName();

  ast::AstNode* PrimaryExpr();
  ast::AstNode* PostfixExpr();
  ast::AstNode* UnaryExpr();
  ast::AstNode* MulExpr();
  ast::AstNode* AddExpr();
  ast::AstNode* RelExpr();
  ast::AstNode* EqExpr();
  ast::AstNode* Expr();
  ast::RetStmt* RetStmt();
  ast::VarDeclStmt* VarDeclStmt();
  ast::AstNode* Stmt();

  ast::If* If();
  ast::Block* Block();
  ast::Array* Array();

  template <typename... Args>
  void Throw(const std::string& fmt, Args... args);
};

std::unique_ptr<ast::File> ParseAst(std::ifstream&);

}  // namespace felis

#endif  // FELIS_SYNTAX_PARSER_H_
