#include <fstream>
#include <iostream>
#include <string>

#include "args.h"
#include "backend/llvm_builder.h"
#include "backend/target.h"
#include "check/type_checker.h"
#include "error/error.h"
#include "loc.h"
#include "printer/ast_printer.h"
#include "syntax/parser.h"
#include "unique.h"

class Session {
 public:
  Session(std::unique_ptr<Opts> opts) : opts(std::move(opts)){};

  bool open(std::ifstream &in) {
    in.open(opts->Filepath());
    if (!in.is_open()) {
      std::cerr << "felisc: failed to open " << opts->Filepath() << std::endl;
      return false;
    }
    return true;
  }

  int Report(felis::CompileError &e) {
    std::cerr << opts->Filepath() << ":" << e.what() << std::endl;
    return 1;
  }

  int Report(felis::LocError &e) {
    std::ifstream in;
    if (!open(in)) {
      return 1;
    }
    felis::Pos pos(in, e.loc);
    std::cerr << opts->Filepath() << ":" << pos.line << ":" << pos.col << ":"
              << e.what() << std::endl;
    return 1;
  }

  std::unique_ptr<felis::ast::File> ParseAst() {
    std::ifstream in;
    if (!open(in)) {
      return nullptr;
    }
    return felis::ParseAst(in);
  }

  std::unique_ptr<llvm::TargetMachine> CreateTargetMachine() {
    std::string err;
    std::unique_ptr<llvm::TargetMachine> machine =
        felis::CreateTargetMachine(opts->Target(), err);
    if (!machine) {
      throw std::runtime_error(err);
    }
    return std::move(machine);
  }

  void Build(std::unique_ptr<felis::LLVMBuilder> builder,
             std::unique_ptr<felis::ast::File> ast) {
    builder->Build(std::move(ast));

    if (opts->IsEmit(EmitType::LLVM_IR)) {
      builder->EmitLLVMIR(opts->OutputName(EmitType::LLVM_IR));
    }
    if (opts->IsEmit(EmitType::LLVM_BC)) {
      builder->EmitLLVMBC(opts->OutputName(EmitType::LLVM_BC));
    }
    if (opts->IsEmit(EmitType::ASM)) {
      builder->EmitASM(opts->OutputName(EmitType::ASM));
    }
    if (opts->IsEmit(EmitType::OBJ)) {
      builder->EmitOBJ(opts->OutputName(EmitType::OBJ));
    }
  }

  int Run() {
    int exit = 0;
    try {
      auto machine = CreateTargetMachine();
      if (!machine) return 1;

      bool is_32bit = machine->getTargetTriple().isArch32Bit();

      auto ast = ParseAst();
      if (!ast) return 1;

      if (opts->IsPrintAst()) felis::AstPrinter().Print(ast);

      felis::TypeMaps type_maps(is_32bit);
      felis::TypeChecker(type_maps).Check(ast);
      type_maps.ResolveTypes();

      Build(std::make_unique<felis::LLVMBuilder>("felis", opts->Filepath(),
                                                 std::move(machine), type_maps),
            std::move(ast));

    } catch (felis::LocError &err) {
      exit = Report(err);
    } catch (felis::CompileError &err) {
      exit = Report(err);
    } catch (std::runtime_error err) {
      std::cerr << err.what() << std::endl;
      exit = 1;
    }
    return exit;
  }

 private:
  std::unique_ptr<Opts> opts;
  std::ifstream in_;
};

int main(int argc, char *argv[]) {
  auto opts = ParseArgs(argc, argv);

  Session sess(std::move(opts));
  return sess.Run();
}
