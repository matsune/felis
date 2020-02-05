#ifndef PARSER_HPP
#define PARSER_HPP

#include <deque>
#include <memory>
#include <string>
#include "ast.hpp"
#include "token.hpp"

#define UNIMPLEMENTED(msg) throw FatalError("unimplemented " #msg);
#define UNREACHABLE(msg) throw FatalError("unreachable " #msg);

class FatalError {
 public:
  string msg;

  FatalError(string msg) : msg(msg){};
};

class Parser {
 private:
  string filename = "";
  deque<unique_ptr<Token>> tokens;

  unique_ptr<Token> &peek();

  unique_ptr<Token> &peek2();

  unique_ptr<Token> bump();

  unique_ptr<Extern> parseExtern();

  unique_ptr<FnDecl> parseFnDecl();

  unique_ptr<FnProto> parseFnProto();

  bool parseFnArgs(vector<unique_ptr<FnArg>> &args);

  unique_ptr<FnArg> parseFnArg();

  unique_ptr<Expr> parseExpr(uint8_t prec = 0);

  unique_ptr<Expr> parsePrimary();

  unique_ptr<Stmt> parseStmt();

  unique_ptr<IfStmt> parseIfStmt();

  unique_ptr<Block> parseBlock();

  template <typename... Args>
  void error(const char *format, Args const &... args);

 public:
  Parser(string filename) : filename(filename){};

  void push_token(unique_ptr<Token> &&token) { tokens.push_back(move(token)); };

  unique_ptr<File> parse();
};

#endif
