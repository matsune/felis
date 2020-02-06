#ifndef FELIS_SYNTAX_PARSER_H_
#define FELIS_SYNTAX_PARSER_H_

#include <deque>
#include <memory>
#include <string>
#include "syntax/ast.h"
#include "syntax/token.h"

#define UNIMPLEMENTED(msg) throw FatalError("unimplemented " #msg);
#define UNREACHABLE(msg) throw FatalError("unreachable " #msg);

namespace felis {

class FatalError {
 public:
  std::string msg;

  FatalError(std::string msg) : msg(msg) {}

 private:
};

class Parser {
 public:
  Parser(std::string filename) : filename(filename) {}

  void push_token(std::unique_ptr<Token> &&token) {
    tokens.push_back(move(token));
  }

  std::unique_ptr<File> parse();

 private:
  std::string filename = "";

  std::deque<std::unique_ptr<Token>> tokens;

  std::unique_ptr<Token> &peek();

  std::unique_ptr<Token> &peek2();

  std::unique_ptr<Token> bump();

  std::unique_ptr<Extern> parseExtern();

  std::unique_ptr<FnDecl> parseFnDecl();

  std::unique_ptr<FnProto> parseFnProto();

  bool parseFnArgs(std::vector<std::unique_ptr<FnArg>> &args);

  std::unique_ptr<FnArg> parseFnArg();

  std::unique_ptr<Expr> parseExpr(uint8_t prec = 0);

  std::unique_ptr<Expr> parsePrimary();

  std::unique_ptr<Stmt> parseStmt();

  std::unique_ptr<IfStmt> parseIfStmt();

  std::unique_ptr<Block> parseBlock();

  template <typename... Args>
  void error(const char *format, Args const &... args);
};

}  // namespace felis

#endif  // FELIS_SYNTAX_PARSER_H_
