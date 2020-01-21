%{
#include <iostream>
#include <string>
#include <memory>
#include "lexer.hpp"

using namespace std;

#undef YY_DECL
#define YY_DECL bool Lexer::yylex(Token &token)
#define YY_USER_ACTION offset += yyleng; lineCols.back() += yyleng;

#undef YY_NULL
#define YY_NULL false

%}

%option nodefault
%option yyclass="Lexer"
%option noyywrap
%option c++

IDENT	[a-zA-Z_][0-9a-zA-Z_]*

%%
%{
  nl = ws = false;
%}

[ \t]+ { 
  ws = true;
}

[ \t\n\r]+ {
  lineCols.back() -= yyleng;
  for (int i = 0; i < yyleng; ++i) {
    ++lineCols.back();
    switch (yytext[i]) {
      case '\n':
      case '\r':
        lineCols.push_back(0);
        break;
      default:
        break;
    }
  }

  nl = true; 
}

{IDENT} { 
  token.sval = move(yytext);
  return makeToken(token, TokenKind::IDENT);
}

[1-9][0-9]* {
  token.ival = strtoull(yytext, 0, 10);
  return makeToken(token, TokenKind::LIT_INT);
}

"true"|"false" {
  token.bval = yytext == "true";
  return makeToken(token, TokenKind::LIT_BOOL);
}

"+"  { return makeToken(token, TokenKind::PLUS); }
"-"  { return makeToken(token, TokenKind::MINUS); }
"*"  { return makeToken(token, TokenKind::STAR); }
"/"  { return makeToken(token, TokenKind::SLASH); }
"%"  { return makeToken(token, TokenKind::PERCENT); }
"&"  { return makeToken(token, TokenKind::AND); }
"|"  { return makeToken(token, TokenKind::OR); }
"^"  { return makeToken(token, TokenKind::CARET); }
"<<" { return makeToken(token, TokenKind::SHL); }
">>" { return makeToken(token, TokenKind::SHR); }
"&&" { return makeToken(token, TokenKind::ANDAND); }
"||" { return makeToken(token, TokenKind::OROR); }
"<"  { return makeToken(token, TokenKind::LT); }
"<=" { return makeToken(token, TokenKind::LE); }
">"  { return makeToken(token, TokenKind::GT); }
">=" { return makeToken(token, TokenKind::GE); }
"==" { return makeToken(token, TokenKind::EQEQ); }
"!=" { return makeToken(token, TokenKind::NEQ); }
"!"  { return makeToken(token, TokenKind::NOT); }
"("  { return makeToken(token, TokenKind::LPAREN); }
")"  { return makeToken(token, TokenKind::RPAREN); }
"{"  { return makeToken(token, TokenKind::LBRACE); }
"}"  { return makeToken(token, TokenKind::RBRACE); }
"="  { return makeToken(token, TokenKind::EQ); }
";"  { return makeToken(token, TokenKind::SEMI); } 
":"  { return makeToken(token, TokenKind::COLON); } 
","  { return makeToken(token, TokenKind::COMMA); } 
"->" { return makeToken(token, TokenKind::ARROW); } 

<<EOF>> {
  return makeToken(token, TokenKind::END);
}

. { 
  Pos p = getPos(offset - yyleng);
  cerr << "Error: illegal character at line "
       << p.line+1 << ", col " << p.column+1 << endl;
  return makeToken(token, TokenKind::END);
}

%%

bool Lexer::makeToken(Token &token, TokenKind kind) {
  token.kind = kind;
  token.nl = nl;
  token.ws = ws;
  token.offset = offset - yyleng;
  token.len = yyleng;
  return kind != TokenKind::END;
}

Pos Lexer::getPos(uint32_t offset) {
  Pos pos;
  for(int i = 0; i < lineCols.size(); ++i) {
    auto lineCol = lineCols[i];
    if (offset < lineCol) {
      pos.columns(offset);
      offset = 0;
      break;
    } else {
      if (i < lineCols.size()-1) {
        pos.lines();
        offset -= lineCol;
      } else {
        pos.columns(offset);
        offset -= lineCol;
        break;
      }
    }
  }
  pos.columns(offset);
  return pos;
}
