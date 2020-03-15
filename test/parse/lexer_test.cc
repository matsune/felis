#include "gtest/gtest.h"
#include "syntax/syntax.h"

#define NEXT(KIND)                \
  t = lexer.Next();               \
  ASSERT_FALSE(lexer.HasError()); \
  ASSERT_EQ(t->kind, KIND)

namespace felis {

TEST(LexerTest, Empty) {
  std::stringstream in;
  in << "";
  Lexer lexer(in);
  auto t = std::make_unique<Token>();
  NEXT(TokenKind::END);
}

TEST(LexerTest, Valid_Ident) {
  std::stringstream in;
  in << "abc あいう aa_あ ";
  Lexer lexer(in);
  std::unique_ptr<Token> t;

  NEXT(TokenKind::IDENT);
  ASSERT_EQ(t->sval, "abc");
  NEXT(TokenKind::IDENT);
  ASSERT_EQ(t->sval, "あいう");
  NEXT(TokenKind::IDENT);
  ASSERT_EQ(t->sval, "aa_あ");
}

TEST(LexerTest, Valid_Int) {
  std::stringstream in;
  in << "0 23_4__5 0b11 0o71";
  Lexer lexer(in);
  std::unique_ptr<Token> t;

  NEXT(TokenKind::LIT_INT);
  ASSERT_EQ(t->ival, 0);
  NEXT(TokenKind::LIT_INT);
  ASSERT_EQ(t->ival, 2345);
  NEXT(TokenKind::LIT_INT);
  ASSERT_EQ(t->ival, 3);
  NEXT(TokenKind::LIT_INT);
  ASSERT_EQ(t->ival, 57);
}

TEST(LexerTest, Valid_Float) {
  std::stringstream in;
  in << "0.27_2 3e-2 8e4";
  Lexer lexer(in);
  std::unique_ptr<Token> t;

  NEXT(TokenKind::LIT_FLOAT);
  ASSERT_EQ(t->fval, 0.272);
  NEXT(TokenKind::LIT_FLOAT);
  ASSERT_EQ(t->fval, 3e-2);
  NEXT(TokenKind::LIT_FLOAT);
  ASSERT_EQ(t->fval, 8e4);
}

TEST(LexerTest, Valid_Char) {
  std::stringstream in;
  in << "'a' '\\'' '\\\\'";
  Lexer lexer(in);
  std::unique_ptr<Token> t;

  NEXT(TokenKind::LIT_CHAR);
  ASSERT_EQ(t->cval, 'a');
  NEXT(TokenKind::LIT_CHAR);
  ASSERT_EQ(t->cval, '\'');
  NEXT(TokenKind::LIT_CHAR);
  ASSERT_EQ(t->cval, '\\');
}

TEST(LexerTest, Valid_String) {
  std::stringstream in;
  in << "\"aaaaaa\" \"アイウエオ\"";
  Lexer lexer(in);
  std::unique_ptr<Token> t;

  NEXT(TokenKind::LIT_STR);
  ASSERT_EQ(t->sval, "aaaaaa");
  NEXT(TokenKind::LIT_STR);
  ASSERT_EQ(t->sval, "アイウエオ");
}

TEST(LexerTest, Valid_KW) {
  std::stringstream in;
  in << "fn let var ret ext if else";
  Lexer lexer(in);
  std::unique_ptr<Token> t;

  NEXT(TokenKind::KW_FN);
  NEXT(TokenKind::KW_LET);
  NEXT(TokenKind::KW_VAR);
  NEXT(TokenKind::KW_RET);
  NEXT(TokenKind::KW_EXT);
  NEXT(TokenKind::KW_IF);
  NEXT(TokenKind::KW_ELSE);
}

TEST(LexerTest, Valid_Symbol) {
  std::stringstream in;
  in << "+ - / * % & | ^ << >> && || < <= > >= == != !(){}=;:,-> /* comment \
  */";
  Lexer lexer(in);
  std::unique_ptr<Token> t;

  NEXT(TokenKind::PLUS);
  NEXT(TokenKind::MINUS);
  NEXT(TokenKind::SLASH);
  NEXT(TokenKind::STAR);
  NEXT(TokenKind::PERCENT);
  NEXT(TokenKind::AND);
  NEXT(TokenKind::OR);
  NEXT(TokenKind::CARET);
  NEXT(TokenKind::SHL);
  NEXT(TokenKind::SHR);
  NEXT(TokenKind::ANDAND);
  NEXT(TokenKind::OROR);
  NEXT(TokenKind::LT);
  NEXT(TokenKind::LE);
  NEXT(TokenKind::GT);
  NEXT(TokenKind::GE);
  NEXT(TokenKind::EQEQ);
  NEXT(TokenKind::NEQ);
  NEXT(TokenKind::NOT);
  NEXT(TokenKind::LPAREN);
  NEXT(TokenKind::RPAREN);
  NEXT(TokenKind::LBRACE);
  NEXT(TokenKind::RBRACE);
  NEXT(TokenKind::EQ);
  NEXT(TokenKind::SEMI);
  NEXT(TokenKind::COLON);
  NEXT(TokenKind::COMMA);
  NEXT(TokenKind::ARROW);
  NEXT(TokenKind::END);
}

}  // namespace felis
