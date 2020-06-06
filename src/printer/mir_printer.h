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

  void PrintAssign(const std::shared_ptr<mir::AssignInst> &);
  void PrintUnary(const std::shared_ptr<mir::UnaryInst> &);
  void PrintBinary(const std::shared_ptr<mir::BinaryInst> &);
  void PrintCmp(const std::shared_ptr<mir::CmpInst> &);
  void PrintCall(const std::shared_ptr<mir::CallInst> &);
  void PrintBr(const std::shared_ptr<mir::BrInst> &);
  void PrintGoto(const std::shared_ptr<mir::GotoInst> &);
  void PrintRet(const std::shared_ptr<mir::RetInst> &);
};

}  // namespace felis

#endif  // FELIS_SYNTAX_MIR_PRINTER_H_
