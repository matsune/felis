#ifndef PRINTER_HPP
#define PRINTER_HPP
#include <memory>
#include "ast.hpp"

class Printer {
  uint16_t depth = 0;
  uint32_t line = 1;
  bool afterNl = false;
  void writeLineNum();
  void indent();
  void write(string msg);
  void writeln(string msg);
  void down(string);
  void up(string);

 public:
  void print(unique_ptr<Node>& node);
};

#endif
