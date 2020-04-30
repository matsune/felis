#include "rune.h"

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

int encoderune(const rune r, char res[4]) {
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

bool appendRune(std::string &str, const rune &r) {
  char ch[4] = {};
  int n = encoderune(r, ch);
  if (!n) {
    return false;
  }
  str.append(ch);
  return true;
}

}  // namespace felis

