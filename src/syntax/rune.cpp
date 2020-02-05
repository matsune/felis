#include "rune.hpp"

const uint8_t TAG_CONT = 0b10000000;
const uint8_t TAG_TWO_B = 0b11000000;
const uint8_t TAG_THREE_B = 0b11100000;
const uint8_t TAG_FOUR_B = 0b11110000;
const uint32_t MAX_ONE_B = 0x80;
const uint32_t MAX_TWO_B = 0x800;
const uint32_t MAX_THREE_B = 0x10000;

int rune::len_utf8() {
  if (scalar < MAX_ONE_B) {
    return 1;
  } else if (scalar < MAX_TWO_B) {
    return 2;
  } else if (scalar < MAX_THREE_B) {
    return 3;
  } else {
    return 4;
  }
};

int rune::encode_utf8(char bytes[4]) {
  if (scalar < MAX_ONE_B) {
    bytes[0] = scalar;
    return 1;
  } else if (scalar < MAX_TWO_B) {
    bytes[0] = (scalar >> 6 & 0x1F) | TAG_TWO_B;
    bytes[1] = (scalar & 0x3F) | TAG_CONT;
    return 2;
  } else if (scalar < MAX_THREE_B) {
    bytes[0] = (scalar >> 12 & 0x0F) | TAG_THREE_B;
    bytes[1] = (scalar >> 6 & 0x3F) | TAG_CONT;
    bytes[2] = (scalar & 0x3F) | TAG_CONT;
    return 3;
  } else {
    bytes[0] = (scalar >> 18 & 0x07) | TAG_FOUR_B;
    bytes[1] = (scalar >> 12 & 0x3F) | TAG_CONT;
    bytes[2] = (scalar >> 6 & 0x3F) | TAG_CONT;
    bytes[3] = (scalar & 0x3F) | TAG_CONT;
    return 4;
  }
};
