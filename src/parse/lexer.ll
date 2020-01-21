%{
#include <iostream>
#include <string>
#include <memory>
#include "lexer.hpp"

using namespace std;

#undef YY_DECL
#define YY_DECL bool Lexer::yylex(Token &token)
#define YY_USER_ACTION offset += yyleng;

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
  lineCols.back() += yyleng;
}

[ \t\n\r]+ {
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
  lineCols.back() += yyleng;
  token.str = move(yytext);
  return makeToken(token, IDENT);
}

[1-9][0-9]* {
  lineCols.back() += yyleng;
  token.num = strtoull(yytext, 0, 10);
  return makeToken(token, LIT_INT);
}

"+"|"-"|"*"|"%"|"&"|"|"|"^"|"{"|"}"|";" { 
  lineCols.back() += yyleng;
  return makeToken(token, yytext[0]);
} 

<<EOF>> {
  return makeToken(token, 0);
}

. { 
  Pos p = getPos(offset - yyleng);
  cerr << "Error: illegal character at line " << p.line+1 << ", col " << p.column+1 << endl;
  return makeToken(token, 0);
}

%%

bool Lexer::makeToken(Token &token, int kind) {
  token.kind = kind;
  token.nl = nl;
  token.ws = ws;
  token.offset = offset - yyleng;
  token.len = yyleng;
  return kind != 0;
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
