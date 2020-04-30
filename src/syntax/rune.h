#ifndef FELIS_SYNTAX_RUNE_H_
#define FELIS_SYNTAX_RUNE_H_

#include <iostream>

namespace felis {

using rune = int32_t;

int encodeRune(const rune, char[4]);
rune consumeRune(std::basic_istream<char> &);
int runeCount(const std::string &);
bool appendRune(std::string &, const rune &);

}  // namespace felis

#endif  // FELIS_SYNTAX_RUNE_H_
