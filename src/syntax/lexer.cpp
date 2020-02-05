#include "lexer.hpp"

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

rune Lexer::scan() {
  rune r;

  int16_t byte1 = in.get();
  if (in.eof() || in.fail()) {
    return r;
  }
  if (byte1 <= 0x7F) {
    // 1 byte
    r.scalar = byte1;
  } else if (byte1 >= 0xF0) {
    // 4 bytes
    uint8_t x = get_tail(byte1, 3);
    uint8_t byte2 = in.get(), byte3 = in.get(), byte4 = in.get();
    uint8_t y = get_tail(byte2, 6);
    uint8_t z = get_tail(byte3, 6);
    uint8_t w = get_tail(byte4, 6);
    r.scalar = x << 18 | y << 12 | z << 6 | w;
  } else if (byte1 >= 0xE0) {
    // 3 bytes
    uint8_t x = get_tail(byte1, 4);
    uint8_t byte2 = in.get(), byte3 = in.get();
    uint8_t y = get_tail(byte2, 6);
    uint8_t z = get_tail(byte3, 6);
    r.scalar = x << 12 | y << 6 | z;
  } else {
    // 2 bytes
    uint8_t x = get_tail(byte1, 5);
    uint8_t byte2 = in.get();
    uint8_t y = get_tail(byte2, 6);
    r.scalar = x << 6 | y;
  }
  return r;
};

rune Lexer::bump() {
  if (peek == '\n') {
    pos.lines();
  } else {
    pos.columns();
  }
  rune tmp = peek;
  peek = scan();
  return tmp;
};

rune Lexer::getPeek() { return peek; };

bool Lexer::bumpIf(uint32_t ch) {
  if (peek.scalar == ch) {
    bump();
    return true;
  }
  return false;
}

bool Lexer::bumpIf(function<bool(uint32_t)> f) {
  if (f(peek.scalar)) {
    bump();
    return true;
  }
  return false;
}

bool Lexer::next(unique_ptr<Token> &t) {
  t->reset();

  while (true) {
    t->pos = pos;
    if (bumpIf(is_space)) {
      t->ws = true;
      continue;
    } else if (bumpIf(is_newline)) {
      t->nl = true;
      continue;
    } else if (bumpIf(0)) {
      return true;
    } else if (bumpIf('\'')) {
      t->kind = TokenKind::LIT_CHAR;
      return eat_char(t->cval);
    } else if (bumpIf('"')) {
      t->kind = TokenKind::LIT_STR;
      return eat_string(t->sval);
    } else if (bumpIf('/')) {
      if (bumpIf('/')) {
        eatLineComment();
        t->nl = true;
        continue;
      } else if (bumpIf('*')) {
        bool hasNl(false);
        if (!eatBlockComment(hasNl)) {
          return false;
        }
        if (hasNl)
          t->nl = true;
        else
          t->ws = true;
        continue;
      } else {
        t->kind = TokenKind::SLASH;
        return true;
      }
    } else if (bumpIf(';')) {
      t->kind = TokenKind::SEMI;
    } else if (bumpIf(',')) {
      t->kind = TokenKind::COMMA;
    } else if (bumpIf('(')) {
      t->kind = TokenKind::LPAREN;
    } else if (bumpIf(')')) {
      t->kind = TokenKind::RPAREN;
    } else if (bumpIf('{')) {
      t->kind = TokenKind::LBRACE;
    } else if (bumpIf('}')) {
      t->kind = TokenKind::RBRACE;
    } else if (bumpIf(':')) {
      t->kind = TokenKind::COLON;
    } else if (bumpIf('=')) {
      if (bumpIf('=')) {
        t->kind = TokenKind::EQEQ;
      } else {
        t->kind = TokenKind::EQ;
      }
    } else if (bumpIf('!')) {
      if (bumpIf('=')) {
        t->kind = TokenKind::NEQ;
      } else {
        t->kind = TokenKind::NOT;
      }
    } else if (bumpIf('<')) {
      if (bumpIf('<')) {
        t->kind = TokenKind::SHL;
      } else if (bumpIf('=')) {
        t->kind = TokenKind::LE;
      } else {
        t->kind = TokenKind::LT;
      }
    } else if (bumpIf('>')) {
      if (bumpIf('>')) {
        t->kind = TokenKind::SHR;
      } else if (bumpIf('=')) {
        t->kind = TokenKind::GE;
      } else {
        t->kind = TokenKind::GT;
      }
    } else if (bumpIf('-')) {
      if (bumpIf('>')) {
        t->kind = TokenKind::ARROW;
      } else {
        t->kind = TokenKind::MINUS;
      }
    } else if (bumpIf('&')) {
      if (bumpIf('&')) {
        t->kind = TokenKind::ANDAND;
      } else {
        t->kind = TokenKind::AND;
      }
    } else if (bumpIf('|')) {
      if (bumpIf('|')) {
        t->kind = TokenKind::OROR;
      } else {
        t->kind = TokenKind::OR;
      }
    } else if (bumpIf('+')) {
      t->kind = TokenKind::PLUS;
    } else if (bumpIf('*')) {
      t->kind = TokenKind::STAR;
    } else if (bumpIf('^')) {
      t->kind = TokenKind::CARET;
    } else if (bumpIf('%')) {
      t->kind = TokenKind::PERCENT;
    } else if (is_decimalc(peek.scalar)) {
      return eat_num(t);
    } else if (is_ident_head(peek.scalar)) {
      return eat_ident(t);
    } else {
      return error("unsupported char %c", peek.scalar);
    }
    return true;
  }
};

bool Lexer::eat_ident(unique_ptr<Token> &t) {
  char bytes[4] = {0};
  int len = bump().encode_utf8(bytes);
  string name(bytes, len);
  while (is_ident_body(peek.scalar)) {
    len = bump().encode_utf8(bytes);
    name.append(bytes, len);
  }
  if (name == "true" || name == "false") {
    t->kind = TokenKind::LIT_BOOL;
    t->bval = name == "true";
  } else if (name == "fn") {
    t->kind = TokenKind::KW_FN;
  } else if (name == "let") {
    t->kind = TokenKind::KW_LET;
  } else if (name == "var") {
    t->kind = TokenKind::KW_VAR;
  } else if (name == "ret") {
    t->kind = TokenKind::KW_RET;
  } else if (name == "ext") {
    t->kind = TokenKind::KW_EXT;
  } else if (name == "if") {
    t->kind = TokenKind::KW_IF;
  } else if (name == "else") {
    t->kind = TokenKind::KW_ELSE;
  } else {
    t->kind = TokenKind::IDENT;
    t->sval = name;
  }
  return true;
}

bool Lexer::read_digits(string &s, bool f(uint32_t)) {
  bool hasDigits(false);
  while (true) {
    if (f(peek.scalar)) {
      hasDigits = true;
      s.push_back(bump().scalar);
    } else if (peek == '_') {
      bump();
    } else {
      break;
    }
  }
  if (!hasDigits) {
    return error("no digits\n");
  }
  return true;
}

bool Lexer::eat_num(unique_ptr<Token> &t) {
  t->kind = TokenKind::LIT_INT;

  auto first = bump();
  string s("");
  s.push_back(first.scalar);
  int base(10);
  if (first == '0') {
    if (bumpIf('b')) {
      // binary
      base = 2;
      if (!read_digits(s, is_bitc)) {
        return false;
      }
    } else if (bumpIf('o')) {
      // octal
      base = 8;
      if (!read_digits(s, is_octalc)) {
        return false;
      }
    } else if (bumpIf('x')) {
      // hex
      base = 16;
      if (!read_digits(s, is_hexc)) {
        return false;
      }
    } else if (is_decimalc(peek.scalar) || peek == '_') {
      while (true) {
        if (is_decimalc(peek.scalar)) {
          s.push_back(bump().scalar);
        } else if (peek == '_') {
          bump();
        } else {
          break;
        }
      }
    } else if (peek == '.' || peek == 'e' || peek == 'E') {
      // do nothing; goto exponent
    } else {
      // just 0
      t->kind = TokenKind::LIT_INT;
      return true;
    }
  } else {
    while (true) {
      if (is_decimalc(peek.scalar)) {
        s.push_back(bump().scalar);
      } else if (peek == '_') {
        bump();
      } else {
        break;
      }
    }
  }

  if (peek == '.' || peek == 'e' || peek == 'E') {
    // fractional part
    if (base != 10) {
      return error("'%c' exponent requires decimal mantissa\n", peek);
    }
    bool dot = peek == '.';
    s.push_back(bump().scalar);
    if (!dot && peek == '-') {
      s.push_back(bump().scalar);
    }
    if (!read_digits(s, is_decimalc)) {
      return false;
    }
    t->kind = TokenKind::LIT_FLOAT;
    t->fval = stold(s);
  } else {
    t->ival = stoull(s, nullptr, base);
    if (t->ival > INT64_MAX) {
      return error("overflow int64 size\n");
    }
  }
  return true;
}

bool Lexer::eat_char(rune &cval) {
  auto first = peek;
  switch (first.scalar) {
    case '\\':
      bump();
      char c;
      if (!escape(c)) {
        return false;
      }
      cval = rune(c);
      break;
    case '\'':
      return error("empty char literal\n");
    case '\t':
    case '\n':
    case '\r':
      return error("escape only char\n");
    default:
      cval = bump();
      break;
  }
  if (peek.scalar != '\'') {
    return error("more than one character\n");
  }
  bump();
  return true;
};

bool Lexer::escape(char &c) {
  if (bumpIf('\'')) {
    c = '\'';
  } else if (bumpIf('"')) {
    c = '"';
  } else if (bumpIf('\\')) {
    c = '\\';
  } else if (bumpIf('0')) {
    c = '\0';
  } else if (bumpIf('a')) {
    c = '\a';
  } else if (bumpIf('b')) {
    c = '\b';
  } else if (bumpIf('f')) {
    c = '\f';
  } else if (bumpIf('n')) {
    c = '\n';
  } else if (bumpIf('r')) {
    c = '\r';
  } else if (bumpIf('t')) {
    c = '\t';
  } else if (bumpIf('v')) {
    c = '\v';
  } else if (bumpIf('x')) {
    if (is_hexc(peek.scalar)) {
      c = hexc(bump().scalar) * 16;
    } else {
      return error("non-hex character '%c'\n", peek.scalar);
    }
    if (is_hexc(peek.scalar)) {
      c += hexc(bump().scalar);
    } else {
      return error("non-hex character '%c'\n", peek.scalar);
    }
  } else if (bumpIf('u')) {
    if (!bumpIf('{')) {
      return error("expected '{'");
    }

    // bumpIf hex chars up to 6 digits
    // and at most 0x10FFFF
    uint32_t val(0);
    int count(0);
    while (true) {
      if (is_hexc(peek.scalar)) {
        if (count > 5) {
          return error(
              "overlong unicode escape (must have at most 6 hex digits)\n");
        }
        val = val * 16 + hexc(bump().scalar);
        count++;
      } else if (bumpIf('}')) {
        break;
      } else {
        return error("invalid character in unicode escape: %c\n", peek.scalar);
      }
    }
    if (count == 0) {
      return error("empty character in unicode escape\n");
    }
    if (val > 0x10FFFF) {
      return error("unicode escape must be at most 10FFFF\n");
    }
    c = val;
  } else {
    return error("unknown escape sequence\n");
  }
  return true;
}

bool Lexer::eat_string(string &sval) {
  bool terminated(false);
  int len;
  char dst[4];
  while (true) {
    auto r = peek;
    if (r == '"') {
      terminated = true;
      bump();
      break;
    } else if (r == 0 || r == '\n' || r == '\r') {
      break;
    } else if (r == '\\') {
      bump();
      char c;
      if (!escape(c)) {
        return false;
      }
      sval.push_back(c);
    } else {
      len = bump().encode_utf8(dst);
      sval.append(dst, len);
    }
  }
  if (!terminated) {
    return error("unterminated string\n");
  }
  return true;
};

void Lexer::eatLineComment() {
  while (peek.scalar != 0 && !is_newline(peek.scalar)) {
    bump();
  }
};

bool Lexer::eatBlockComment(bool &hasNl) {
  int depth(1);
  while (true) {
    if (peek == 0) {
      break;
    } else if (bumpIf('/')) {
      if (bump() == '*') {
        depth++;
      }
    } else if (bumpIf('*')) {
      if (bump() == '/') {
        depth--;
      }
      if (depth == 0) {
        break;
      }
    } else {
      if (is_newline(peek.scalar)) {
        hasNl = true;
      }
      bump();
    }
  }
  if (depth != 0) {
    return error("unterminated block comment\n");
  }
  return true;
};

template <typename... Args>
bool Lexer::error(const char *format, Args const &... args) {
  fprintf(stderr, "%s:%d:%d: ", filename.c_str(), pos.line, pos.column);
  fprintf(stderr, format, args...);
  return false;
};