#ifndef FELIS_CHECK_PARSE_H_
#define FELIS_CHECK_PARSE_H_

#include <string>

namespace felis {

bool ParseInt(const std::string &lit, int64_t &n, std::string &err);
bool ParseFloat(const std::string &lit, double &n, std::string &err);

}  // namespace felis

#endif  // FELIS_CHECK_PARSE_H_
