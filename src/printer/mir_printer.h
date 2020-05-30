#ifndef FELIS_SYNTAX_MIR_PRINTER_H_
#define FELIS_SYNTAX_MIR_PRINTER_H_

#include <iostream>
#include <memory>
#include <string>

#include "node/mir.h"
#include "printer/printer.h"

namespace felis {

class MirPrinter : public Printer<mir::File> {
 public:
  void Print(const std::unique_ptr<mir::File> &) override;

 private:
  void PrintFunc(const std::shared_ptr<mir::Func> &);
  void PrintStore(const std::shared_ptr<mir::Store> &);
  void PrintLoad(const std::shared_ptr<mir::Load> &);
  void PrintBinary(const std::shared_ptr<mir::Binary> &);
  void PrintComp(const std::shared_ptr<mir::Comp> &);
  void PrintUnary(const std::shared_ptr<mir::Unary> &);
  void PrintCall(const std::shared_ptr<mir::Call> &);
  void PrintRet(const std::shared_ptr<mir::Ret> &);
  void PrintArray(const std::shared_ptr<mir::Array> &);
  void PrintCond(const std::shared_ptr<mir::Cond> &);
};

}  // namespace felis

#endif  // FELIS_SYNTAX_MIR_PRINTER_H_
