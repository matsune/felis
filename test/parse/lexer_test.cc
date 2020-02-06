#include "gtest/gtest.h"
#include "syntax/syntax.h"

#define NEXT(KIND)            \
  ASSERT_TRUE(lexer.next(t)); \
  ASSERT_EQ(t->kind, KIND);

namespace felis {

TEST(LexerTest, EmptyInput) {
  std::stringstream in;
  in << "";
  Lexer lexer(in);
  auto t = std::make_unique<Token>();
  NEXT(TokenKind::END)
}

TEST(LexerTest, lex) {
  std::stringstream in;
  in << "abcあ 12_21 23.1 true false 'a' \"string\"";
  in << "fn let var ret ext if else ";
  in << "+ - / * % & | ^ << >> && || < <= > >= == != !(){}=;:,-> /* comment */";
  Lexer lexer(in);
  auto t = std::make_unique<Token>();

  NEXT(TokenKind::IDENT)
  ASSERT_EQ(t->sval, "abcあ");
  ASSERT_FALSE(t->ws);
  ASSERT_FALSE(t->nl);

  NEXT(TokenKind::LIT_INT)
  ASSERT_EQ(t->ival, 1221);
  ASSERT_TRUE(t->ws);
  ASSERT_FALSE(t->nl);

  NEXT(TokenKind::LIT_FLOAT)
  ASSERT_EQ(t->fval, 23.1);

  NEXT(TokenKind::LIT_BOOL)
  ASSERT_EQ(t->bval, true);

  NEXT(TokenKind::LIT_BOOL)
  ASSERT_EQ(t->bval, false);

  NEXT(TokenKind::LIT_CHAR)
  ASSERT_EQ(t->cval, 'a');

  NEXT(TokenKind::LIT_STR)
  ASSERT_EQ(t->sval, "string");

  NEXT(TokenKind::KW_FN)
  NEXT(TokenKind::KW_LET)
  NEXT(TokenKind::KW_VAR)
  NEXT(TokenKind::KW_RET)
  NEXT(TokenKind::KW_EXT)
  NEXT(TokenKind::KW_IF)
  NEXT(TokenKind::KW_ELSE)

  NEXT(TokenKind::PLUS)
  NEXT(TokenKind::MINUS)
  NEXT(TokenKind::SLASH)
  NEXT(TokenKind::STAR)
  NEXT(TokenKind::PERCENT)
  NEXT(TokenKind::AND)
  NEXT(TokenKind::OR)
  NEXT(TokenKind::CARET)
  NEXT(TokenKind::SHL)
  NEXT(TokenKind::SHR)
  NEXT(TokenKind::ANDAND)
  NEXT(TokenKind::OROR)
  NEXT(TokenKind::LT)
  NEXT(TokenKind::LE)
  NEXT(TokenKind::GT)
  NEXT(TokenKind::GE)
  NEXT(TokenKind::EQEQ)
  NEXT(TokenKind::NEQ)
  NEXT(TokenKind::NOT)
  NEXT(TokenKind::LPAREN)
  NEXT(TokenKind::RPAREN)
  NEXT(TokenKind::LBRACE)
  NEXT(TokenKind::RBRACE)
  NEXT(TokenKind::EQ)
  NEXT(TokenKind::SEMI)
  NEXT(TokenKind::COLON)
  NEXT(TokenKind::COMMA)
  NEXT(TokenKind::ARROW)
  NEXT(TokenKind::END)
}

}  // namespace felis
