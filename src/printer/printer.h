#ifndef FELIS_SYNTAX_PRINTER_H_
#define FELIS_SYNTAX_PRINTER_H_

#include <iostream>
#include <memory>

#include "loc.h"
#include "node/node.h"

namespace felis {

template <class T>
class Printer {
 public:
  virtual void Print(const std::unique_ptr<T> &) = 0;

 protected:
  uint16_t depth_ = 0;
  uint32_t line_ = 1;
  bool after_nl_ = false;

  void Indent() {
    for (int i = 0; i < depth_; i++) {
      std::cout << ". ";
    }
  }

  void WriteLineNum() { printf("%4d ", line_); }

  template <typename... Args>
  void Write(const std::string format, Args const &... args) {
    if (after_nl_) {
      Indent();
    }
    printf(format.c_str(), args...);
    after_nl_ = false;
  }

  template <typename... Args>
  void Writeln(const std::string format, Args const &... args) {
    if (after_nl_) {
      Indent();
    }
    printf(format.c_str(), args...);
    std::cout << std::endl;
    line_++;
    WriteLineNum();
    after_nl_ = true;
  }

  void Down(std::string s) {
    Writeln(s);
    depth_++;
  }

  void Up(std::string s) {
    depth_--;
    Writeln(s);
  }

  void PrintPtr(Node *ptr) { Writeln("Address: %p", ptr); }
};

}  // namespace felis

#endif  // FELIS_SYNTAX_PRINTER_H_
