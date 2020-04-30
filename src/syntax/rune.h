#ifndef FELIS_SYNTAX_RUNE_H_
#define FELIS_SYNTAX_RUNE_H_

#include <iostream>

namespace felis {

using rune = int32_t;

int encoderune(const rune r, char res[4]);
bool appendRune(std::string &str, const rune &r);

}  // namespace felis

#endif  // FELIS_SYNTAX_RUNE_H_
