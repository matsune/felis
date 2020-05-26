#include <fstream>
#include <iostream>
#include <string>

#include "args.h"
#include "builder/builder.h"
#include "builder/target.h"
#include "check/lower.h"
#include "error/error.h"
#include "loc.h"
#include "printer/ast_printer.h"
#include "printer/hir_printer.h"
#include "syntax/lexer.h"
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

  felis::unique_deque<felis::Token> ParseTokens(std::ifstream &in) {
    felis::unique_deque<felis::Token> tokens;
    felis::Lexer lexer(in);
    bool is_end(false);
    while (!is_end) {
      auto token = lexer.Next();
      is_end = token->kind == felis::Token::Kind::END;
      tokens.push_back(std::move(token));
    }
    return std::move(tokens);
  }

  std::unique_ptr<felis::ast::File> ParseAst() {
    std::ifstream in;
    if (!open(in)) {
      return nullptr;
    }
    auto tokens = ParseTokens(in);
    return felis::Parser(std::move(tokens)).Parse();
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

  void Build(std::unique_ptr<llvm::TargetMachine> machine,
             std::unique_ptr<felis::hir::File> hir) {
    auto builder = std::make_unique<felis::Builder>("felis", opts->Filepath(),
                                                    std::move(machine));
    builder->Build(std::move(hir));

    if (opts->IsEmit(EmitType::LLVM_IR)) {
      builder->EmitLLVMIR(opts->OutputName(EmitType::LLVM_IR));
    }
    if (opts->IsEmit(EmitType::LLVM_BC)) {
      builder->EmitLLVMBC(opts->OutputName(EmitType::LLVM_BC));
    }
    if (opts->IsEmit(EmitType::ASM)) {
      builder->EmitASM(opts->OutputName(EmitType::ASM));
    }
    bool emit_obj = opts->IsEmit(EmitType::OBJ);
    bool emit_link = opts->IsEmit(EmitType::LINK);

    bool has_obj = false;
    std::string obj_path = opts->OutputName(EmitType::OBJ);
    if (emit_obj || emit_link) {
      builder->EmitOBJ(obj_path);
      has_obj = true;
    }
    if (emit_link) {
      std::string out = opts->OutputName(EmitType::LINK);
      std::string s = "gcc " + obj_path + " -o " + out;
      system(s.c_str());
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

      auto hir = felis::Lowering(std::move(ast), is_32bit);
      if (!hir) return 1;

      /* if (opts->IsPrintHir()) felis::HirPrinter().Print(hir); */

      /* Build(std::move(machine), std::move(hir)); */

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
