#ifndef FELIS_STRING_STRING_H_
#define FELIS_STRING_STRING_H_

#include <cstdio>
#include <string>
#include <vector>

#include "check/decl.h"
#include "check/type.h"
#include "node/ast.h"
#include "syntax/token.h"

namespace felis {

template <typename... Args>
std::string format(const std::string &fmt, Args... args) {
  size_t len = std::snprintf(nullptr, 0, fmt.c_str(), args...);
  std::vector<char> buf(len + 1);
  std::snprintf(&buf[0], len + 1, fmt.c_str(), args...);
  return std::string(&buf[0], &buf[0] + len);
}

std::string ToString(const Token::Kind &);
std::string ToString(const ast::BinaryOp::Kind &);
std::string ToString(const Type::Kind &);
std::string ToString(const Type);
std::string ToString(const Decl::Kind &);
std::string ToString(const std::shared_ptr<Decl> &);

}  // namespace felis

#endif  // FELIS_STRING_STRING_H_
