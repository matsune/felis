%{
#include <iostream>
#include <string>
#include <memory>
#include "lexer.hpp"

using namespace std;

#undef YY_DECL
#define YY_DECL bool Lexer::yylex(Token &token)
#define YY_USER_ACTION offset += yyleng; src.columns(yyleng);

#undef YY_NULL
#define YY_NULL false

void unescape(string& str);

%}

%option nodefault
%option yyclass="Lexer"
%option noyywrap
%option c++

IDENT	[a-zA-Z_][0-9a-zA-Z_]*
ESCAPED \\[0abfnrtv?\\'"]|\\x[0-9a-zA-Z]{2}

%%
%{
  nl = ws = false;
%}

[ \t]+ { 
  ws = true;
}

[ \t\n\r]+ {
  src.columns(-yyleng);
  for (int i = 0; i < yyleng; ++i) {
    src.columns();
    switch (yytext[i]) {
      case '\n':
      case '\r':
        src.line();
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

'({ESCAPED}|[^\\'])+' {
  string str(yytext+1,yyleng-2);
  unescape(str);
  if (str.size() > 1) {
    YY_FATAL_ERROR(
        "unsupported multibyte char" );
  }
  token.ival = str[0];
  return makeToken(token, TokenKind::LIT_CHAR);
}

\"({ESCAPED}|[^\\"])*\" {
  string str(yytext+1,yyleng-2);
  unescape(str);
  token.sval = str;
  return makeToken(token, TokenKind::LIT_STR);
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
  Pos p = src.getPos(offset - yyleng);
  cerr << "Error: illegal character at line "
       << p.line+1 << ", col " << p.column+1 << endl;
  return makeToken(token, TokenKind::UNKNOWN);
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

inline int hexc(int c)
{
  if (c>='0' && c<='9') return c-'0';
  if (c>='a' && c<='f') return 10+c-'a';
  if (c>='A' && c<='F') return 10+c-'A';
  return -1;
}

void unescape(string& str)
{
  int sz = str.size();
  int d=0;
  for (int s=0; s<sz; ++s,++d) {
    if (str[s] != '\\') {
      str[d] = str[s];
      continue;
    }

    ++s;

    switch (str[s]) {
      case 'a': str[d] = '\a'; break;
      case 'b': str[d] = '\b'; break;
      case 'n': str[d] = '\n'; break;
      case 'r': str[d] = '\r'; break;
      case 't': str[d] = '\t'; break;
      case 'v': str[d] = '\v'; break;
      case '0': str[d] = '\0'; break;
      case 'x': 
        {
          int h1 = hexc(str[s+1]), h2 = hexc(str[s+2]);
          if (h1 >= 0 && h2 >= 0) {
            str[d] = 16*h1 + h2; s+=2;
          } else {
            str[d] = 'x';
          }
        } 
        break;
      default: str[d] = str[s];
    } 
  }
  str.resize(d);
}

