#include <llvm/ADT/StringMap.h>
#include <llvm/MC/SubtargetFeature.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>

#include <fstream>
#include <iostream>
#include <string>

#include "args.h"
#include "builder/builder.h"
#include "check/decl_checker.h"
#include "check/lower.h"
#include "check/ty_infer.h"
#include "error/error.h"
#include "loc.h"
#include "printer/ast_printer.h"
#include "printer/hir_printer.h"
#include "syntax/lexer.h"
#include "syntax/parser.h"
#include "unique.h"

std::string getHostCPUFeatures() {
  llvm::SubtargetFeatures Features;
  llvm::StringMap<bool> HostFeatures;

  if (llvm::sys::getHostCPUFeatures(HostFeatures))
    for (auto &F : HostFeatures) Features.AddFeature(F.first(), F.second);

  return Features.getString();
}

std::unique_ptr<llvm::TargetMachine> createTargetMachine(std::string &err) {
  if (llvm::InitializeNativeTarget()) return nullptr;
  if (llvm::InitializeNativeTargetAsmPrinter()) return nullptr;

  std::string triple = llvm::sys::getDefaultTargetTriple();
  std::string cpu = llvm::sys::getHostCPUName();
  std::string features = getHostCPUFeatures();
  const llvm::Target *target = llvm::TargetRegistry::lookupTarget(triple, err);
  if (!target) {
    return nullptr;
  }

  llvm::TargetOptions opt;
  return std::unique_ptr<llvm::TargetMachine>(
      target->createTargetMachine(triple, cpu, features, opt, llvm::None));
}

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

  std::unique_ptr<felis::hir::File> LowerAst(
      std::unique_ptr<felis::ast::File> ast) {
    std::map<felis::ast::AstNode *, std::shared_ptr<felis::Decl>> ast_decl;
    std::map<felis::ast::AstNode *, int> node_ty_id_;
    felis::DeclChecker checker(ast_decl);
    checker.SetupBuiltin();
    checker.Check(ast);
    felis::TyInfer infer(ast_decl);
    infer.Infer(ast);

    for (auto &it : infer.ty_map) {
      std::cout << it.first << " " << ToString(it.second.get()) << std::endl;
    }
    std::cout << "--------------" << std::endl;
    for (auto &it : ast_decl) {
      std::cout << it.first << " | ";
      it.second->Debug();
    }
    return nullptr;
    /* return felis::Lower(ast_decl).Lowering(std::move(ast)); */
  }

  std::unique_ptr<llvm::TargetMachine> CreateTargetMachine() {
    std::string err;
    auto machine = createTargetMachine(err);
    if (!machine) {
      std::cerr << opts->Filepath() << ":" << err << std::endl;
      return nullptr;
    }
    return std::move(machine);
  }

  std::unique_ptr<felis::Builder> Build(
      std::unique_ptr<llvm::TargetMachine> machine,
      std::unique_ptr<felis::hir::File> hir) {
    auto builder = std::make_unique<felis::Builder>("felis", opts->Filepath(),
                                                    std::move(machine));
    builder->Build(std::move(hir));
    return std::move(builder);
  }

  void Emit(std::unique_ptr<felis::Builder> builder) {
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
      auto ast = ParseAst();
      if (!ast) return 1;

      if (opts->IsPrintAst()) felis::AstPrinter().Print(ast);

      auto hir = LowerAst(std::move(ast));
      if (!hir) return 1;

      felis::HirPrinter().Print(hir);

      /* auto machine = CreateTargetMachine(); */
      /* if (!machine) return 1; */

      /* auto builder = Build(std::move(machine), std::move(hir)); */
      /* if (!builder) return 1; */

      /* Emit(std::move(builder)); */

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
