#include "lexer.hpp"

#define check(f) \
  if (!f) return false;

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
  bool nl, ws;
  while (true) {
    auto c = bump();
    switch (c.val) {
      case '"':
        check(eat_string(t.sval));
        t.kind = TokenKind::LIT_STR;
        return true;
      case 0:
        return false;
      case '\n':
      case '\r':
        nl = true;
        break;
      default:
        return error("unsupported char %c", c.val);
    }
  }
};

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
        case 'u':
          return error("not implemented unicode literal\n");
        default:
          return error("unknown escape sequence\n");
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
