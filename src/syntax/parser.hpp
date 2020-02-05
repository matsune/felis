#ifndef PARSER_HPP
#define PARSER_HPP

#include <deque>
#include <memory>
#include <string>
#include "ast.hpp"
#include "token.hpp"

class Parser {
 private:
  string filename = "";
  deque<unique_ptr<Token>> tokens;
  void error(string msg);
  unique_ptr<Token> &peek();
  unique_ptr<Token> &peek2();
  unique_ptr<Token> bump();
  template <typename... Args>
  void error(const char *format, Args const &... args);
  unique_ptr<Expr> parseExpr(uint8_t prec = 0);
  unique_ptr<Expr> parsePrimary();
  unique_ptr<Stmt> parseStmt();
  unique_ptr<Block> parseBlock();
  unique_ptr<IfStmt> parseIfStmt();

 public:
  void push_token(unique_ptr<Token> &&token) { tokens.push_back(move(token)); };
  unique_ptr<File> parse();
  void setFilename(string filename) { this->filename = filename; }
};

#endif
