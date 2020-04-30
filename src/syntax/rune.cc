#include "rune.h"

#include <sstream>

namespace felis {

const uint32_t maxRune = 0x10ffff;
const uint16_t surrogateMin = 0xd800;
const uint16_t surrogateMax = 0xdfff;
const uint32_t rune1Max = (1 << 7) - 1;
const uint32_t rune2Max = (1 << 11) - 1;
const uint32_t rune3Max = (1 << 16) - 1;
const uint8_t t1 = 0x00;
const uint8_t tx = 0x80;
const uint8_t t2 = 0xC0;
const uint8_t t3 = 0xE0;
const uint8_t t4 = 0xF0;
const uint8_t maskx = 0x3f;

int encodeRune(const rune r, char res[4]) {
  if (r <= rune1Max) {
    res[0] = r;
    return 1;
  } else if (r <= rune2Max) {
    res[0] = t2 | char(r >> 6);
    res[1] = tx | (char(r) & maskx);
    return 2;
  } else if (r > maxRune || (surrogateMin <= r && r <= surrogateMax)) {
    // invalid
    return 0;
  } else if (r <= rune3Max) {
    res[0] = t3 | char(r >> 12);
    res[1] = tx | (char(r >> 6) & maskx);
    res[2] = tx | (char(r) & maskx);
    return 3;
  } else {
    res[0] = t4 | char(r >> 18);
    res[1] = tx | (char(r >> 12) & maskx);
    res[2] = tx | (char(r >> 6) & maskx);
    res[3] = tx | (char(r) & maskx);
    return 4;
  }
}

inline uint8_t get_tail(uint8_t byte, uint8_t width) {
  return byte & (0xff >> (8 - width));
}

rune consumeRune(std::basic_istream<char> &stream) {
  int16_t byte1 = stream.get();
  if (stream.eof() || stream.fail()) {
    return 0;
  }
  if (byte1 <= 0x7F) {
    // 1 byte
    return byte1;
  } else if (byte1 >= 0xF0) {
    // 4 bytes
    uint8_t x = get_tail(byte1, 3);
    uint8_t byte2 = stream.get(), byte3 = stream.get(), byte4 = stream.get();
    uint8_t y = get_tail(byte2, 6);
    uint8_t z = get_tail(byte3, 6);
    uint8_t w = get_tail(byte4, 6);
    return x << 18 | y << 12 | z << 6 | w;
  } else if (byte1 >= 0xE0) {
    // 3 bytes
    uint8_t x = get_tail(byte1, 4);
    uint8_t byte2 = stream.get(), byte3 = stream.get();
    uint8_t y = get_tail(byte2, 6);
    uint8_t z = get_tail(byte3, 6);
    return x << 12 | y << 6 | z;
  } else {
    // 2 bytes
    uint8_t x = get_tail(byte1, 5);
    uint8_t byte2 = stream.get();
    uint8_t y = get_tail(byte2, 6);
    return x << 6 | y;
  }
}

int runeCount(const std::string &str) {
  std::stringstream ss(str);
  int count(0);
  while (!consumeRune(ss)) count++;
  return count;
}

bool appendRune(std::string &str, const rune &r) {
  char ch[4] = {};
  int n = encodeRune(r, ch);
  if (!n) {
    return false;
  }
  str.append(ch);
  return true;
}

}  // namespace felis

