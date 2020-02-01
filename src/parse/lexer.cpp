#include "lexer.hpp"
#include <string>

inline bool is_newline(int c) { return c == '\n' || c == '\r'; }
inline bool is_space(int c) {
  return c == ' ' || c == '\t' || c == 0x09 || c == 0x0c;
}
inline bool is_bitc(int c) { return c == '0' || c == '1'; }
inline bool is_octalc(int c) { return c >= '0' && c <= '7'; }
inline bool is_decimalc(int c) { return c >= '0' && c <= '9'; }
inline bool is_alphabet(int c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}
inline bool is_hexc(int c) {
  return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') ||
         (c >= 'A' && c <= 'F');
}
inline int hexc(int c) {
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
    r.val = byte1;
    r.len_utf8 = 1;
    r.bytes[0] = byte1;
  } else if (byte1 >= 0xF0) {
    // 4 bytes
    uint8_t x = get_tail(byte1, 3);
    uint8_t byte2 = in.get(), byte3 = in.get(), byte4 = in.get();
    uint8_t y = get_tail(byte2, 6);
    uint8_t z = get_tail(byte3, 6);
    uint8_t w = get_tail(byte4, 6);
    r.val = x << 18 | y << 12 | z << 6 | w;
    r.len_utf8 = 4;
    r.bytes[0] = byte1;
    r.bytes[1] = byte2;
    r.bytes[2] = byte3;
    r.bytes[3] = byte4;
  } else if (byte1 >= 0xE0) {
    // 3 bytes
    uint8_t x = get_tail(byte1, 4);
    uint8_t byte2 = in.get(), byte3 = in.get();
    uint8_t y = get_tail(byte2, 6);
    uint8_t z = get_tail(byte3, 6);
    r.val = x << 12 | y << 6 | z;
    r.len_utf8 = 3;
    r.bytes[0] = byte1;
    r.bytes[1] = byte2;
    r.bytes[2] = byte3;
  } else {
    // 2 bytes
    uint8_t x = get_tail(byte1, 5);
    uint8_t byte2 = in.get();
    uint8_t y = get_tail(byte2, 6);
    r.val = x << 6 | y;
    r.len_utf8 = 3;
    r.bytes[0] = byte1;
    r.bytes[1] = byte2;
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

bool Lexer::next(Token &t) {
  t.reset();

  while (true) {
    t.pos = pos;
    auto c = peek;
    if (is_space(c.val)) {
      bump();
      t.ws = true;
    } else if (is_newline(c.val)) {
      bump();
      t.nl = true;
    } else if (c.val == 0) {
      return false;
    } else if (c == '\'') {
      bump();
      t.kind = TokenKind::LIT_CHAR;
      return eat_char(t.ival);
    } else if (c == '"') {
      bump();
      t.kind = TokenKind::LIT_STR;
      return eat_string(t.sval);
    } else if (is_decimalc(c.val)) {
      return eat_num(t);
    } else if (is_ident_head(c.val)) {
      return eat_ident(t);
    } else {
      return error("unsupported char %c", c.val);
    }
  }
};

bool Lexer::eat_ident(Token &t) {
  string name;
  name.append(bump().bytes);
  while (is_ident_body(peek.val)) {
    name.append(bump().bytes);
  }
  if (name == "true" || name == "false") {
    t.kind = TokenKind::LIT_BOOL;
    t.bval = name == "true";
  } else {
    t.kind = TokenKind::IDENT;
    t.sval = name;
  }
  return true;
}

bool Lexer::read_digits(string &s, bool f(int)) {
  bool hasDigits;
  while (true) {
    if (f(peek.val)) {
      hasDigits = true;
      s.push_back(bump().val);
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

bool Lexer::eat_num(Token &t) {
  t.kind = TokenKind::LIT_INT;

  auto first = bump();
  string s;
  s.push_back(first.val);
  int base(10);
  if (first == '0') {
    if (peek == 'b') {
      // binary
      bump();
      base = 2;
      if (!read_digits(s, is_bitc)) {
        return false;
      }
    } else if (peek == 'o') {
      // octal
      bump();
      base = 8;
      if (!read_digits(s, is_octalc)) {
        return false;
      }
    } else if (peek == 'x') {
      // hex
      bump();
      base = 16;
      if (!read_digits(s, is_hexc)) {
        return false;
      }
    } else if (is_decimalc(peek.val) || peek == '_') {
      if (!read_digits(s, is_decimalc)) {
        return false;
      }
    } else if (peek == '.' || peek == 'e' || peek == 'E') {
      // goto exponent
    } else {
      // just 0
      t.kind = TokenKind::LIT_INT;
      return true;
    }
  } else {
    if (!read_digits(s, is_decimalc)) {
      return false;
    }
  }

  if (peek == '.' || peek == 'e' || peek == 'E') {
    // fractional part
    if (base != 10) {
      return error("'%c' exponent requires decimal mantissa\n", peek);
    }
    bool dot = peek == '.';
    s.push_back(bump().val);
    if (!dot && peek == '-') {
      s.push_back(bump().val);
    }
    if (!read_digits(s, is_decimalc)) {
      return false;
    }
    t.kind = TokenKind::LIT_FLOAT;
    t.fval = stold(s);
  } else {
    t.ival = stoull(s, nullptr, base);
    if (t.ival > INT64_MAX) {
      return error("overflow int64 size\n");
    }
  }
  return true;
}

bool Lexer::eat_char(uint64_t &ival) {
  auto first = peek;
  switch (first.val) {
    case '\\':
      bump();
      char c;
      if (!escape(c)) {
        return false;
      }
      ival = c;
      break;
    case '\'':
      return error("empty char literal\n");
    case '\t':
    case '\n':
    case '\r':
      return error("escape only char\n");
    default:
      ival = bump().val;
      break;
  }
  if (peek.val != '\'') {
    return error("more than one character\n");
  }
  bump();
  return true;
};

bool Lexer::escape(char &c) {
  switch (peek.val) {
    case '\'':
      bump();
      c = '\'';
      break;
    case '"':
      bump();
      c = '"';
      break;
    case '\\':
      bump();
      c = '\\';
      break;
    case '0':
      bump();
      c = '\0';
      break;
    case 'a':
      bump();
      c = '\a';
      break;
    case 'b':
      bump();
      c = '\b';
      break;
    case 'f':
      bump();
      c = '\f';
      break;
    case 'n':
      bump();
      c = '\n';
      break;
    case 'r':
      bump();
      c = '\r';
      break;
    case 't':
      bump();
      c = '\t';
      break;
    case 'v':
      bump();
      c = '\v';
      break;
    case 'x':
      bump();
      if (is_hexc(peek.val)) {
        c = hexc(bump().val) * 16;
      } else {
        return error("non-hex character '%c'\n", peek.val);
      }
      if (is_hexc(peek.val)) {
        c += hexc(bump().val);
      } else {
        return error("non-hex character '%c'\n", peek.val);
      }
      break;
    case 'u': {
      bump();

      if (peek != '{') {
        return error("expected '{'");
      }
      bump();

      // read hex chars up to 6 digits
      // and at most 0x10FFFF
      uint32_t val(0);
      int count(0);
      while (true) {
        if (is_hexc(peek.val)) {
          if (count > 5) {
            return error(
                "overlong unicode escape (must have at most 6 hex digits)\n");
          }
          val = val * 16 + hexc(bump().val);
          count++;
        } else if (peek.val == '}') {
          bump();
          break;
        } else {
          return error("invalid character in unicode escape: %c\n", peek.val);
        }
      }
      if (count == 0) {
        return error("empty character in unicode escape\n");
      }
      if (val > 0x10FFFF) {
        return error("unicode escape must be at most 10FFFF\n");
      }
      c = val;
    } break;

    default:
      return error("unknown escape sequence\n");
  }
  return true;
}

bool Lexer::eat_string(string &sval) {
  bool terminated(false);
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
      bump();
      sval.append(r.bytes);
    }
  }
  if (!terminated) {
    return error("unterminated string\n");
  }
  return true;
};

template <typename... Args>
bool Lexer::error(const char *format, Args const &... args) {
  fprintf(stderr, "%s:%d:%d: ", filename.c_str(), pos.line, pos.column);
  fprintf(stderr, format, args...);
  return false;
};
