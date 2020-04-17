#include "syntax/lexer.h"

namespace felis {

namespace {

inline bool is_newline(uint32_t c) { return c == 0x0A || c == 0x0D; }
inline bool is_space(uint32_t c) {
  return c == 0x09 || c == 0x0B || c == 0x0C || c == 0x20;
}
inline bool is_bitc(uint32_t c) { return c == '0' || c == '1'; }
inline bool is_octalc(uint32_t c) { return c >= '0' && c <= '7'; }
inline bool is_decimalc(uint32_t c) { return c >= '0' && c <= '9'; }
inline bool is_alphabet(uint32_t c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}
inline bool is_hexc(uint32_t c) {
  return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') ||
         (c >= 'A' && c <= 'F');
}
inline int hexc(uint32_t c) {
  if (c >= '0' && c <= '9') return c - '0';
  if (c >= 'a' && c <= 'f') return 10 + c - 'a';
  if (c >= 'A' && c <= 'F') return 10 + c - 'A';
  return -1;
}
inline uint8_t get_tail(uint8_t byte, uint8_t width) {
  return byte & (0xff >> (8 - width));
}

inline bool is_ident_head(uint32_t c) {
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

inline bool is_ident_body(uint32_t c) {
  return is_decimalc(c) || is_ident_head(c) || (0x0300 <= c && c <= 0x036F) ||
         (0x1DC0 <= c && c <= 0x1DFF) || (0x20D0 <= c && c <= 0x20FF) ||
         (0xFE20 <= c && c <= 0xFE2F);
}

}  // namespace

rune Lexer::Scan() {
  rune r;

  int16_t byte1 = in_.get();
  if (in_.eof() || in_.fail()) {
    return r;
  }
  if (byte1 <= 0x7F) {
    // 1 byte
    r.scalar = byte1;
  } else if (byte1 >= 0xF0) {
    // 4 bytes
    uint8_t x = get_tail(byte1, 3);
    uint8_t byte2 = in_.get(), byte3 = in_.get(), byte4 = in_.get();
    uint8_t y = get_tail(byte2, 6);
    uint8_t z = get_tail(byte3, 6);
    uint8_t w = get_tail(byte4, 6);
    r.scalar = x << 18 | y << 12 | z << 6 | w;
  } else if (byte1 >= 0xE0) {
    // 3 bytes
    uint8_t x = get_tail(byte1, 4);
    uint8_t byte2 = in_.get(), byte3 = in_.get();
    uint8_t y = get_tail(byte2, 6);
    uint8_t z = get_tail(byte3, 6);
    r.scalar = x << 12 | y << 6 | z;
  } else {
    // 2 bytes
    uint8_t x = get_tail(byte1, 5);
    uint8_t byte2 = in_.get();
    uint8_t y = get_tail(byte2, 6);
    r.scalar = x << 6 | y;
  }
  return r;
}

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

bool Lexer::BumpIf(uint32_t ch) {
  if (peek_.scalar == ch) {
    Bump();
    return true;
  }
  return false;
}

bool Lexer::BumpIf(std::function<bool(uint32_t)> f) {
  if (f(peek_.scalar)) {
    Bump();
    return true;
  }
  return false;
}

char Lexer::Escape() {
  char c;
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
    if (is_hexc(peek_.scalar)) {
      c = hexc(Bump().scalar) * 16;
    } else {
      Throw("non-hex character '%c'\n", peek_.scalar);
    }
    if (is_hexc(peek_.scalar)) {
      c += hexc(Bump().scalar);
    } else {
      Throw("non-hex character '%c'\n", peek_.scalar);
    }
  } else if (BumpIf('u')) {
    if (!BumpIf('{')) {
      Throw("expected '{'");
    }

    // BumpIf hex chars up to 6 digits
    // and at most 0x10FFFF
    uint32_t val(0);
    int count(0);
    while (true) {
      if (is_hexc(peek_.scalar)) {
        if (count > 5) {
          Throw("overlong unicode escape (must have at most 6 hex digits)");
        }
        val = val * 16 + hexc(Bump().scalar);
        count++;
      } else if (BumpIf('}')) {
        break;
      } else {
        Throw("invalid character in unicode escape: %c\n", peek_.scalar);
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

rune Lexer::EatChar() {
  rune r;
  switch (peek_.scalar) {
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
  if (peek_.scalar != '\'') {
    Throw("more than one character");
  }
  Bump();
  return r;
}

void Lexer::EatString(std::string &str) {
  bool terminated(false);
  int len;
  char dst[4];

  while (true) {
    if (peek_ == '"') {
      terminated = true;
      Bump();
      break;
    } else if (peek_ == 0 || peek_ == '\n' || peek_ == '\r') {
      break;
    } else if (BumpIf('\\')) {
      Bump();
      str.push_back(Escape());
    } else {
      len = Bump().encode_utf8(dst);
      str.append(dst, len);
    }
  }
  if (!terminated) {
    Throw("unterminated string");
  }
}

std::unique_ptr<Token> Lexer::Next() {
  auto tok = new Token;

  while (true) {
    tok->pos = pos_;
    if (BumpIf(is_space)) {
      tok->ws = true;
      continue;
    } else if (BumpIf(is_newline)) {
      tok->nl = true;
      continue;
    } else if (BumpIf(0)) {
      tok->kind = TokenKind::END;
    } else if (BumpIf('\'')) {
      tok->kind = TokenKind::LIT_CHAR;
      tok->cval = EatChar();
    } else if (BumpIf('"')) {
      tok->kind = TokenKind::LIT_STR;
      EatString(tok->sval);
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
        tok->kind = TokenKind::SLASH;
      }
    } else if (BumpIf(';')) {
      tok->kind = TokenKind::SEMI;
    } else if (BumpIf(',')) {
      tok->kind = TokenKind::COMMA;
    } else if (BumpIf('(')) {
      tok->kind = TokenKind::LPAREN;
    } else if (BumpIf(')')) {
      tok->kind = TokenKind::RPAREN;
    } else if (BumpIf('{')) {
      tok->kind = TokenKind::LBRACE;
    } else if (BumpIf('}')) {
      tok->kind = TokenKind::RBRACE;
    } else if (BumpIf(':')) {
      tok->kind = TokenKind::COLON;
    } else if (BumpIf('=')) {
      if (BumpIf('=')) {
        tok->kind = TokenKind::EQEQ;
      } else {
        tok->kind = TokenKind::EQ;
      }
    } else if (BumpIf('!')) {
      if (BumpIf('=')) {
        tok->kind = TokenKind::NEQ;
      } else {
        tok->kind = TokenKind::NOT;
      }
    } else if (BumpIf('<')) {
      if (BumpIf('<')) {
        tok->kind = TokenKind::SHL;
      } else if (BumpIf('=')) {
        tok->kind = TokenKind::LE;
      } else {
        tok->kind = TokenKind::LT;
      }
    } else if (BumpIf('>')) {
      if (BumpIf('>')) {
        tok->kind = TokenKind::SHR;
      } else if (BumpIf('=')) {
        tok->kind = TokenKind::GE;
      } else {
        tok->kind = TokenKind::GT;
      }
    } else if (BumpIf('-')) {
      if (BumpIf('>')) {
        tok->kind = TokenKind::ARROW;
      } else {
        tok->kind = TokenKind::MINUS;
      }
    } else if (BumpIf('&')) {
      if (BumpIf('&')) {
        tok->kind = TokenKind::ANDAND;
      } else {
        tok->kind = TokenKind::AND;
      }
    } else if (BumpIf('|')) {
      if (BumpIf('|')) {
        tok->kind = TokenKind::OROR;
      } else {
        tok->kind = TokenKind::OR;
      }
    } else if (BumpIf('+')) {
      tok->kind = TokenKind::PLUS;
    } else if (BumpIf('*')) {
      tok->kind = TokenKind::STAR;
    } else if (BumpIf('^')) {
      tok->kind = TokenKind::CARET;
    } else if (BumpIf('%')) {
      tok->kind = TokenKind::PERCENT;
    } else if (is_decimalc(peek_.scalar)) {
      auto t = EatNum();
      tok->kind = t->kind;
      if (t->kind == TokenKind::LIT_INT) {
        tok->ival = t->ival;
      } else if (t->kind == TokenKind::LIT_FLOAT) {
        tok->fval = t->fval;
      } else {
        // unreachable
      }
    } else if (is_ident_head(peek_.scalar)) {
      auto t = EatIdent();
      tok->kind = t->kind;
      switch (t->kind) {
        case TokenKind::LIT_BOOL:
          tok->bval = t->bval;
          break;
        default:
          tok->sval = t->sval;
          break;
      }
    } else {
      Throw("unsupported char %c", peek_.scalar);
    }
    return std::unique_ptr<Token>(tok);
  }
}

std::unique_ptr<Token> Lexer::EatIdent() {
  char bytes[4] = {0};
  int len = Bump().encode_utf8(bytes);
  std::string name(bytes, len);
  while (is_ident_body(peek_.scalar)) {
    len = Bump().encode_utf8(bytes);
    name.append(bytes, len);
  }
  auto tok = new Token;
  if (name == "true" || name == "false") {
    tok->kind = TokenKind::LIT_BOOL;
    tok->bval = name == "true";
  } else if (name == "fn") {
    tok->kind = TokenKind::KW_FN;
  } else if (name == "let") {
    tok->kind = TokenKind::KW_LET;
  } else if (name == "var") {
    tok->kind = TokenKind::KW_VAR;
  } else if (name == "ret") {
    tok->kind = TokenKind::KW_RET;
  } else if (name == "ext") {
    tok->kind = TokenKind::KW_EXT;
  } else if (name == "if") {
    tok->kind = TokenKind::KW_IF;
  } else if (name == "else") {
    tok->kind = TokenKind::KW_ELSE;
  } else {
    tok->kind = TokenKind::IDENT;
    tok->sval = name;
  }
  return std::unique_ptr<Token>(tok);
}

void Lexer::EatDigits(std::string &str, std::function<bool(uint32_t)> f) {
  bool hasDigits(false);
  while (true) {
    if (f(peek_.scalar)) {
      hasDigits = true;
      str.push_back(Bump().scalar);
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

std::unique_ptr<Token> Lexer::EatNum() {
  TokenKind kind(TokenKind::LIT_INT);

  auto first = Bump();
  std::string str("");
  str.push_back(first.scalar);
  int base(10);
  if (first == '0') {
    if (BumpIf('b')) {
      // binary
      base = 2;
      EatDigits(str, is_bitc);
    } else if (BumpIf('o')) {
      // octal
      base = 8;
      EatDigits(str, is_octalc);
    } else if (BumpIf('x')) {
      // hex
      base = 16;
      EatDigits(str, is_hexc);
    } else if (is_decimalc(peek_.scalar) || peek_ == '_') {
      while (true) {
        if (is_decimalc(peek_.scalar)) {
          str.push_back(Bump().scalar);
        } else if (peek_ == '_') {
          Bump();
        } else {
          break;
        }
      }
    } else if (peek_ == '.' || peek_ == 'e' || peek_ == 'E') {
      // do nothing; goto exponent
    } else {
      // just 0
      return std::make_unique<Token>(kind);
    }
  } else {
    while (true) {
      if (is_decimalc(peek_.scalar)) {
        str.push_back(Bump().scalar);
      } else if (peek_ == '_') {
        Bump();
      } else {
        break;
      }
    }
  }

  if (peek_ == '.' || peek_ == 'e' || peek_ == 'E') {
    // fractional part
    if (base != 10) {
      Throw("'%c' exponent requires decimal mantissa\n", peek_);
    }
    bool dot = peek_ == '.';
    str.push_back(Bump().scalar);
    if (!dot && peek_ == '-') {
      str.push_back(Bump().scalar);
    }

    EatDigits(str, is_decimalc);

    auto tok = new Token(TokenKind::LIT_FLOAT);
    tok->fval = stold(str);
    return std::unique_ptr<Token>(tok);
  } else {
    uint64_t ival = stoull(str, nullptr, base);
    if (ival > INT64_MAX) {
      Throw("overflow int64 size");
    }
    auto tok = new Token(TokenKind::LIT_INT);
    tok->ival = ival;
    return std::unique_ptr<Token>(tok);
  }
}

void Lexer::EatLineComment() {
  while (peek_.scalar != 0 && !is_newline(peek_.scalar)) {
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
      if (is_newline(peek_.scalar)) {
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
  throw CompileError::CreatePosFmt(pos_, fmt, args...);
}

}  // namespace felis
