#ifndef PRINTER_HPP
#define PRINTER_HPP
#include <memory>
#include "ast.hpp"

class Printer {
 public:
  void print(unique_ptr<Node> node);
};

#endif
