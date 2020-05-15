#ifndef FELIS_SYNTAX_RUNE_H_
#define FELIS_SYNTAX_RUNE_H_

#include <fstream>
#include <iostream>
#include <string>

namespace felis {

namespace {

const uint32_t CODE_POINT_MAX = 0x10ffff;
const uint16_t SURROGATE_MIN = 0xd800;
const uint16_t SURROGATE_MAX = 0xdfff;
const uint32_t RUNE1_MAX = (1 << 7) - 1;
const uint32_t RUNE2_MAX = (1 << 11) - 1;
const uint32_t RUNE3_MAX = (1 << 16) - 1;
const uint8_t TAIL_HEAD = 0x80;
const uint8_t T2_HEAD = 0xC0;
const uint8_t T3_HEAD = 0xE0;
const uint8_t T4_HEAD = 0xF0;
const uint8_t TAIL_MASK = 0x3f;

template <typename u8>
inline uint8_t get_tail(u8 v, uint8_t width) {
  return v & (0xff >> (8 - width));
}

template <typename octet>
inline uint8_t mask8(octet v) {
  return 0xff & v;
}

template <typename u8>
inline uint8_t num_bytes_from_first(u8 first) {
  uint8_t m = mask8(first);
  if (m < 0x80) {
    return 1;
  } else if ((m >> 5) == 0b110) {
    return 2;
  } else if ((m >> 4) == 0b1110) {
    return 3;
  } else if ((m >> 3) == 0b11110) {
    return 4;
  } else {
    return 0;
  }
}

template <typename u16>
inline bool is_surrogate(u16 cp) {
  return cp >= SURROGATE_MIN && cp <= SURROGATE_MAX;
}

template <typename u32>
inline bool is_valid_code_point(u32 cp) {
  return cp <= CODE_POINT_MAX && !is_surrogate(cp);
}

template <typename octet>
inline bool is_trail(octet v) {
  return (mask8(v) >> 6) == 0b10;
}

class invalid_rune_error : public std::exception {
 public:
  invalid_rune_error(std::string msg) : msg(msg) {}

  virtual const char *what() const noexcept override { return msg.c_str(); }

 private:
  std::string msg;
};

char get_byte(std::istream &in) {
  char c;
  in.get(c);
  if (in.eof()) {
    return 0;
  }
  if (in.fail() || in.bad()) {
    throw invalid_rune_error("invalid input");
  }
  return c;
}

char get_tail_byte(std::istream &in) {
  char c = get_byte(in);
  if (!is_trail(c)) throw invalid_rune_error("incomplete bytes");
  return c;
}

}  // namespace

struct rune {
  rune(int32_t cp = 0) : cp(cp) {
    if (!is_valid_code_point(cp)) {
      throw invalid_rune_error("invalid code point " + std::to_string(cp));
    }
  }

  void operator=(const int32_t &v) {
    if (!is_valid_code_point(v)) {
      throw invalid_rune_error("invalid code point " + std::to_string(v));
    }
    cp = v;
  }

  void operator+=(const int32_t &v) {
    int32_t tmp = cp + v;
    if (!is_valid_code_point(tmp)) {
      throw invalid_rune_error("invalid code point " + std::to_string(tmp));
    }
    cp = tmp;
  }

  operator int32_t() const { return cp; }

  operator std::string() const {
    char c[4] = {};
    encode(c);
    return std::string(c);
  }

  friend std::ostream &operator<<(std::ostream &out, const rune &r) {
    char c[4] = {};
    r.encode(c);
    out << c;
    return out;
  }

  friend std::istream &operator>>(std::istream &in, rune &r) {
    char byte1 = get_byte(in);
    if (byte1 == 0) {
      r = 0;
      return in;
    }
    uint8_t numBytes = num_bytes_from_first(byte1);
    switch (numBytes) {
      case 0:
        throw invalid_rune_error("invalid first byte " + std::to_string(byte1));
      case 1:
        r = byte1;
        break;
      case 2: {
        uint8_t byte2 = get_tail_byte(in);
        uint8_t x = get_tail(byte1, 5);
        uint8_t y = get_tail(byte2, 6);
        r = x << 6 | y;
      } break;
      case 3: {
        uint8_t byte2 = get_tail_byte(in);
        uint8_t byte3 = get_tail_byte(in);
        uint8_t x = get_tail(byte1, 4);
        uint8_t y = get_tail(byte2, 6);
        uint8_t z = get_tail(byte3, 6);
        r = x << 12 | y << 6 | z;
      } break;
      case 4: {
        uint8_t byte2 = get_tail_byte(in);
        uint8_t byte3 = get_tail_byte(in);
        uint8_t byte4 = get_tail_byte(in);
        uint8_t x = get_tail(byte1, 3);
        uint8_t y = get_tail(byte2, 6);
        uint8_t z = get_tail(byte3, 6);
        uint8_t w = get_tail(byte4, 6);
        r = x << 18 | y << 12 | z << 6 | w;
      } break;
    }
    return in;
  }

  int encode(char res[4]) const {
    int8_t numBytes = bytes();
    switch (numBytes) {
      case 1: {
        res[0] = cp;
      } break;
      case 2: {
        res[0] = T2_HEAD | mask8(cp >> 6);
        res[1] = TAIL_HEAD | (mask8(cp) & TAIL_MASK);
      } break;
      case 3: {
        res[0] = T3_HEAD | mask8(cp >> 12);
        res[1] = TAIL_HEAD | (mask8(cp >> 6) & TAIL_MASK);
        res[2] = TAIL_HEAD | (mask8(cp) & TAIL_MASK);
      } break;
      case 4: {
        res[0] = T4_HEAD | mask8(cp >> 18);
        res[1] = TAIL_HEAD | (mask8(cp >> 12) & TAIL_MASK);
        res[2] = TAIL_HEAD | (mask8(cp >> 6) & TAIL_MASK);
        res[3] = TAIL_HEAD | (mask8(cp) & TAIL_MASK);
      } break;
    }
    return numBytes;
  }

  int8_t bytes() const {
    if (cp <= RUNE1_MAX) {
      return 1;
    } else if (cp <= RUNE2_MAX) {
      return 2;
    } else if (cp <= RUNE3_MAX) {
      return 3;
    } else {
      return 4;
    }
  }

 private:
  int32_t cp;
};

}  // namespace felis

#endif  // FELIS_SYNTAX_RUNE_H_
