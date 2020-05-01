#include "syntax/lexer.h"

namespace felis {

namespace {

inline bool is_newline(rune c) { return c == 0x0A || c == 0x0D; }
inline bool is_space(rune c) {
  return c == 0x09 || c == 0x0B || c == 0x0C || c == 0x20;
}
inline bool is_bitc(rune c) { return c == '0' || c == '1'; }
inline bool is_octalc(rune c) { return c >= '0' && c <= '7'; }
inline bool is_decimalc(rune c) { return c >= '0' && c <= '9'; }
inline bool is_alphabet(rune c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}
inline bool is_hexc(rune c) {
  return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') ||
         (c >= 'A' && c <= 'F');
}
inline int hexc(rune c) {
  if (c >= '0' && c <= '9') return c - '0';
  if (c >= 'a' && c <= 'f') return 10 + c - 'a';
  if (c >= 'A' && c <= 'F') return 10 + c - 'A';
  return -1;
}

inline bool is_ident_head(rune c) {
  return is_alphabet(c) || c == '_' || c == 0x00A8 || c == 0x00AA ||
         c == 0x00AD || c == 0x00AF || (0x00B2 <= c && c <= 0x00B5) ||
         (0x00B7 <= c && c <= 0x00BA) || (0x00BC <= c && c <= 0x00BE) ||
         (0x00C0 <= c && c <= 0x00D6) || (0x00D8 <= c && c <= 0x00F6) ||
         (0x00F8 <= c && c <= 0x00FF) || (0x0100 <= c && c <= 0x02FF) ||
         (0x0370 <= c && c <= 0x167F) || (0x1681 <= c && c <= 0x180D) ||
         (0x180F <= c && c <= 0x1DBF) || (0x1E00 <= c && c <= 0x1FFF) ||
         (0x200B <= c && c <= 0x200D) || (0x202A <= c && c <= 0x202E) ||
         (0x203F <= c && c <= 0x2040) || c == 0x2054 ||
         (0x2060 <= c && c <= 0x206F) || (0x2070 <= c && c <= 0x20CF) ||
         (0x2100 <= c && c <= 0x218F) || (0x2460 <= c && c <= 0x24FF) ||
         (0x2776 <= c && c <= 0x2793) || (0x2C00 <= c && c <= 0x2DFF) ||
         (0x2E80 <= c && c <= 0x2FFF) || (0x3004 <= c && c <= 0x3007) ||
         (0x3021 <= c && c <= 0x302F) || (0x3031 <= c && c <= 0x303F) ||
         (0x3040 <= c && c <= 0xD7FF) || (0xF900 <= c && c <= 0xFD3D) ||
         (0xFD40 <= c && c <= 0xFDCF) || (0xFDF0 <= c && c <= 0xFE1F) ||
         (0xFE30 <= c && c <= 0xFE44) || (0xFE47 <= c && c <= 0xFFFD) ||
         (0x10000 <= c && c <= 0x1FFFD) || (0x20000 <= c && c <= 0x2FFFD) ||
         (0x30000 <= c && c <= 0x3FFFD) || (0x40000 <= c && c <= 0x4FFFD) ||
         (0x50000 <= c && c <= 0x5FFFD) || (0x60000 <= c && c <= 0x6FFFD) ||
         (0x70000 <= c && c <= 0x7FFFD) || (0x80000 <= c && c <= 0x8FFFD) ||
         (0x90000 <= c && c <= 0x9FFFD) || (0xA0000 <= c && c <= 0xAFFFD) ||
         (0xB0000 <= c && c <= 0xBFFFD) || (0xC0000 <= c && c <= 0xCFFFD) ||
         (0xD0000 <= c && c <= 0xDFFFD) || (0xE0000 <= c && c <= 0xEFFFD);
}

inline bool is_ident_body(rune c) {
  return is_decimalc(c) || is_ident_head(c) || (0x0300 <= c && c <= 0x036F) ||
         (0x1DC0 <= c && c <= 0x1DFF) || (0x20D0 <= c && c <= 0x20FF) ||
         (0xFE20 <= c && c <= 0xFE2F);
}

}  // namespace

rune Lexer::Scan() { return consumeRune(in_); }

rune Lexer::Bump() {
  if (peek_ == '\n') {
    pos_.Lines();
  } else {
    pos_.Columns();
  }
  rune tmp = peek_;
  peek_ = Scan();
  return tmp;
}

bool Lexer::BumpIf(char ch) {
  if (peek_ == ch) {
    Bump();
    return true;
  }
  return false;
}

bool Lexer::BumpIf(std::function<bool(rune)> f) {
  if (f(peek_)) {
    Bump();
    return true;
  }
  return false;
}

rune Lexer::Escape() {
  rune c;
  if (BumpIf('\'')) {
    c = '\'';
  } else if (BumpIf('"')) {
    c = '"';
  } else if (BumpIf('\\')) {
    c = '\\';
  } else if (BumpIf('0')) {
    c = '\0';
  } else if (BumpIf('a')) {
    c = '\a';
  } else if (BumpIf('b')) {
    c = '\b';
  } else if (BumpIf('f')) {
    c = '\f';
  } else if (BumpIf('n')) {
    c = '\n';
  } else if (BumpIf('r')) {
    c = '\r';
  } else if (BumpIf('t')) {
    c = '\t';
  } else if (BumpIf('v')) {
    c = '\v';
  } else if (BumpIf('x')) {
    if (is_hexc(peek_)) {
      c = hexc(Bump()) * 16;
    } else {
      Throw("non-hex character '%c'\n", peek_);
    }
    if (is_hexc(peek_)) {
      c += hexc(Bump());
    } else {
      Throw("non-hex character '%c'\n", peek_);
    }
  } else if (BumpIf('u')) {
    if (!BumpIf('{')) {
      Throw("expected '{'");
    }

    // BumpIf hex chars up to 6 digits
    // and at most 0x10FFFF
    rune val(0);
    int count(0);
    while (true) {
      if (is_hexc(peek_)) {
        if (count > 5) {
          Throw("overlong unicode escape (must have at most 6 hex digits)");
        }
        val = val * 16 + hexc(Bump());
        count++;
      } else if (BumpIf('}')) {
        break;
      } else {
        Throw("invalid character in unicode escape: %c\n", peek_);
      }
    }
    if (count == 0) {
      Throw("empty character in unicode escape");
    }
    if (val > 0x10FFFF) {
      Throw("unicode escape must be at most 10FFFF");
    }
    c = val;
  } else {
    Throw("unknown escape sequence");
  }
  return c;
}

void Lexer::EatChar(std::string &val) {
  rune r;
  switch (peek_) {
    case '\\': {
      Bump();
      r = Escape();
    } break;
    case '\'':
      Throw("empty char literal");
    case '\t':
    case '\n':
    case '\r':
      Throw("escape only char");
    default:
      r = Bump();
      break;
  }
  if (peek_ != '\'') {
    Throw("more than one character");
  }
  if (!appendRune(val, r)) {
    Throw("invalid utf8");
  }
  Bump();
}

void Lexer::EatString(std::string &str) {
  bool terminated(false);

  while (true) {
    if (peek_ == '"') {
      terminated = true;
      Bump();
      break;
    } else if (peek_ == 0 || peek_ == '\n' || peek_ == '\r') {
      break;
    } else if (BumpIf('\\')) {
      Bump();
      appendRune(str, Escape());
    } else {
      if (!appendRune(str, Bump())) {
        Throw("invalid utf8");
      }
    }
  }
  if (!terminated) {
    Throw("unterminated string");
  }
}

std::unique_ptr<Token> Lexer::Next() {
  auto tok = std::make_unique<Token>();

  while (true) {
    tok->pos = pos_;
    if (BumpIf(is_space)) {
      tok->ws = true;
      continue;
    } else if (BumpIf(is_newline)) {
      tok->nl = true;
      continue;
    } else if (BumpIf(0)) {
      tok->kind = Token::Kind::END;
    } else if (BumpIf('\'')) {
      tok->kind = Token::Kind::LIT_CHAR;
      EatChar(tok->val);
    } else if (BumpIf('"')) {
      tok->kind = Token::Kind::LIT_STR;
      EatString(tok->val);
    } else if (BumpIf('/')) {
      if (BumpIf('/')) {
        // skip comment
        EatLineComment();
        tok->nl = true;
        continue;
      } else if (BumpIf('*')) {
        // skip comment
        auto hasNl = EatBlockComment();
        if (hasNl)
          tok->nl = true;
        else
          tok->ws = true;
        continue;
      } else {
        tok->kind = Token::Kind::SLASH;
      }
    } else if (BumpIf(';')) {
      tok->kind = Token::Kind::SEMI;
    } else if (BumpIf(',')) {
      tok->kind = Token::Kind::COMMA;
    } else if (BumpIf('(')) {
      tok->kind = Token::Kind::LPAREN;
    } else if (BumpIf(')')) {
      tok->kind = Token::Kind::RPAREN;
    } else if (BumpIf('{')) {
      tok->kind = Token::Kind::LBRACE;
    } else if (BumpIf('}')) {
      tok->kind = Token::Kind::RBRACE;
    } else if (BumpIf(':')) {
      tok->kind = Token::Kind::COLON;
    } else if (BumpIf('=')) {
      if (BumpIf('=')) {
        tok->kind = Token::Kind::EQEQ;
      } else {
        tok->kind = Token::Kind::EQ;
      }
    } else if (BumpIf('!')) {
      if (BumpIf('=')) {
        tok->kind = Token::Kind::NEQ;
      } else {
        tok->kind = Token::Kind::NOT;
      }
    } else if (BumpIf('<')) {
      if (BumpIf('<')) {
        tok->kind = Token::Kind::SHL;
      } else if (BumpIf('=')) {
        tok->kind = Token::Kind::LE;
      } else {
        tok->kind = Token::Kind::LT;
      }
    } else if (BumpIf('>')) {
      if (BumpIf('>')) {
        tok->kind = Token::Kind::SHR;
      } else if (BumpIf('=')) {
        tok->kind = Token::Kind::GE;
      } else {
        tok->kind = Token::Kind::GT;
      }
    } else if (BumpIf('-')) {
      if (BumpIf('>')) {
        tok->kind = Token::Kind::ARROW;
      } else {
        tok->kind = Token::Kind::MINUS;
      }
    } else if (BumpIf('&')) {
      if (BumpIf('&')) {
        tok->kind = Token::Kind::ANDAND;
      } else {
        tok->kind = Token::Kind::AND;
      }
    } else if (BumpIf('|')) {
      if (BumpIf('|')) {
        tok->kind = Token::Kind::OROR;
      } else {
        tok->kind = Token::Kind::OR;
      }
    } else if (BumpIf('+')) {
      tok->kind = Token::Kind::PLUS;
    } else if (BumpIf('*')) {
      tok->kind = Token::Kind::STAR;
    } else if (BumpIf('^')) {
      tok->kind = Token::Kind::CARET;
    } else if (BumpIf('%')) {
      tok->kind = Token::Kind::PERCENT;
    } else if (is_decimalc(peek_)) {
      EatNum(tok);
    } else if (is_ident_head(peek_)) {
      EatIdent(tok);
    } else {
      Throw("unsupported char %c", peek_);
    }
    return std::move(tok);
  }
}

void Lexer::EatIdent(std::unique_ptr<Token> &tok) {
  appendRune(tok->val, Bump());
  while (is_ident_body(peek_)) {
    appendRune(tok->val, Bump());
  }
  if (tok->val == "true" || tok->val == "false") {
    tok->kind = Token::Kind::LIT_BOOL;
  } else if (tok->val == "fn") {
    tok->kind = Token::Kind::KW_FN;
  } else if (tok->val == "let") {
    tok->kind = Token::Kind::KW_LET;
  } else if (tok->val == "var") {
    tok->kind = Token::Kind::KW_VAR;
  } else if (tok->val == "ret") {
    tok->kind = Token::Kind::KW_RET;
  } else if (tok->val == "ext") {
    tok->kind = Token::Kind::KW_EXT;
  } else if (tok->val == "if") {
    tok->kind = Token::Kind::KW_IF;
  } else if (tok->val == "else") {
    tok->kind = Token::Kind::KW_ELSE;
  } else {
    tok->kind = Token::Kind::IDENT;
  }
}

void Lexer::EatDigits(std::string &str, std::function<bool(rune)> f) {
  bool hasDigits(false);
  while (true) {
    if (f(peek_)) {
      hasDigits = true;
      appendRune(str, Bump());
    } else if (peek_ == '_') {
      Bump();
    } else {
      break;
    }
  }
  if (!hasDigits) {
    Throw("no digits");
  }
}

void Lexer::EatNum(std::unique_ptr<Token> &tok) {
  tok->kind = Token::Kind::LIT_INT;
  auto first = Bump();
  appendRune(tok->val, first);
  bool canExponent = true;
  if (first == '0') {
    if (BumpIf('b')) {
      // TODO:
      Throw("unimplemented binary literal");

      // binary
      canExponent = false;
      EatDigits(tok->val, is_bitc);
    } else if (BumpIf('o')) {
      // TODO:
      Throw("unimplemented octal literal");

      // octal
      canExponent = false;
      EatDigits(tok->val, is_octalc);
    } else if (BumpIf('x')) {
      // TODO:
      Throw("unimplemented hex literal");

      // hex
      canExponent = false;
      EatDigits(tok->val, is_hexc);
    } else if (is_decimalc(peek_) || peek_ == '_') {
      while (true) {
        if (is_decimalc(peek_)) {
          appendRune(tok->val, Bump());
        } else if (peek_ == '_') {  // skip
          Bump();
        } else {
          break;
        }
      }
    } else if (peek_ == '.' || peek_ == 'e' || peek_ == 'E') {
      // through
    } else {
      // just 0
      return;
    }
  } else {
    while (true) {
      if (is_decimalc(peek_)) {
        appendRune(tok->val, Bump());
      } else if (peek_ == '_') {
        Bump();
      } else {
        break;
      }
    }
  }

  if (peek_ == '.' || peek_ == 'e' || peek_ == 'E') {
    bool isDot = peek_ == '.';
    if (!isDot) {  // TODO:
      Throw("unimplemented float literal");
    }

    // only decimal has fractional part
    if (!canExponent) {
      Throw("'%c' exponent requires decimal mantissa\n", peek_);
    }
    tok->kind = Token::Kind::LIT_FLOAT;
    appendRune(tok->val, Bump());
    if (!isDot && peek_ == '-') {
      appendRune(tok->val, Bump());
    }
    EatDigits(tok->val, is_decimalc);
  }
}

void Lexer::EatLineComment() {
  while (peek_ != 0 && !is_newline(peek_)) {
    Bump();
  }
}

bool Lexer::EatBlockComment() {
  int depth(1);
  bool hasNl(false);

  while (true) {
    if (peek_ == 0) {
      break;
    } else if (BumpIf('/')) {
      if (Bump() == '*') {
        depth++;
      }
    } else if (BumpIf('*')) {
      if (Bump() == '/') {
        depth--;
      }
      if (depth == 0) {
        break;
      }
    } else {
      if (is_newline(peek_)) {
        hasNl = true;
      }
      Bump();
    }
  }
  if (depth != 0) {
    Throw("unterminated block comment");
  }
  return hasNl;
}

template <typename... Args>
void Lexer::Throw(const std::string &fmt, Args... args) {
  throw CompileError::CreatePos(pos_, fmt, args...);
}

}  // namespace felis
