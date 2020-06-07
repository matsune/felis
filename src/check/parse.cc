
#include "check/parse.h"

#include <stdexcept>

namespace felis {

bool ParseInt(const std::string &lit, int64_t &n, std::string &err) {
  try {
    n = stoll(lit);
    return true;
  } catch (std::out_of_range e) {
    err = "out of range";
  } catch (std::invalid_argument e) {
    err = "invalid int literal";
  }
  return false;
}

bool ParseFloat(const std::string &lit, double &n, std::string &err) {
  try {
    // TODO: parse float
    n = stod(lit);
    return true;
  } catch (std::out_of_range e) {
    err = "out of range";
  } catch (std::invalid_argument e) {
    err = "invalid float literal";
  }
  return false;
}

}  // namespace felis
