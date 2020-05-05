#include "builder.h"

#include <llvm/ADT/StringMap.h>
#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/MC/SubtargetFeature.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>

#include "ptr.h"

namespace felis {

std::string getHostCPUFeatures() {
  llvm::SubtargetFeatures Features;
  llvm::StringMap<bool> HostFeatures;

  if (llvm::sys::getHostCPUFeatures(HostFeatures))
    for (auto& F : HostFeatures) Features.AddFeature(F.first(), F.second);

  return Features.getString();
}

bool Builder::CreateTargetMachine(std::string& err) {
  if (llvm::InitializeNativeTarget()) return false;

  std::string triple = llvm::sys::getDefaultTargetTriple();
  std::string cpu = llvm::sys::getHostCPUName();
  std::string features = getHostCPUFeatures();
  const llvm::Target* target = llvm::TargetRegistry::lookupTarget(triple, err);
  if (!target) {
    return false;
  }

  llvm::Reloc::Model rm;
  llvm::TargetOptions opt;
  machine_ = std::unique_ptr<llvm::TargetMachine>(
      target->createTargetMachine(triple, cpu, features, opt, rm));
  return true;
}

llvm::Type* Builder::GetLLVMTyFromTy(std::shared_ptr<Type> ty) {
  switch (ty->TypeKind()) {
    case Type::Kind::BOOL:
      return llvm::Type::getInt1Ty(ctx_);
    case Type::Kind::CHAR:
      return llvm::Type::getInt32Ty(ctx_);
    case Type::Kind::I32:
      return llvm::Type::getInt32Ty(ctx_);
    case Type::Kind::I64:
      return llvm::Type::getInt64Ty(ctx_);
    case Type::Kind::F32:
      return llvm::Type::getFloatTy(ctx_);
    case Type::Kind::F64:
      return llvm::Type::getDoubleTy(ctx_);
    case Type::Kind::STRING:
      std::cerr << "unimplemented string" << std::endl;
      exit(1);
    case Type::Kind::FUNC: {
      auto funcType = (FuncType*)ty.get();
      std::vector<llvm::Type*> args;
      for (auto& arg : funcType->args) {
        args.push_back(GetLLVMTyFromTy(arg));
      }
      return llvm::FunctionType::get(GetLLVMTyFromTy(funcType->ret), args,
                                     false);
    } break;
    case Type::Kind::VOID:
      return llvm::Type::getVoidTy(ctx_);
    default:
      std::cerr << "unimplemented ty" << std::endl;
      exit(1);
  }
}

void Builder::Build(std::unique_ptr<hir::File> file) {
  for (auto& ext : file->externs) {
    declMap_[ext->decl] = BuildFnProto(ext->decl);
  }
  for (auto& fnDecl : file->fnDecls) {
    auto func = BuildFnProto(fnDecl->decl);
    auto it = func->arg_begin();
    for (int i = 0; i < fnDecl->args.size(); i++) {
      std::shared_ptr<Decl> arg = fnDecl->args.at(i);
      auto name = arg->name;
      it->setName(name);
      declMap_[arg] = it;
      it++;
    }
    declMap_[fnDecl->decl] = func;
  }

  while (!file->fnDecls.empty()) {
    auto fnDecl = std::move(file->fnDecls.front());
    file->fnDecls.pop_front();
    auto func = (llvm::Function*)declMap_[fnDecl->decl];
    currentFunc_ = func;
    auto bb = llvm::BasicBlock::Create(ctx_, "", func);
    builder_.SetInsertPoint(bb);

    BuildBlock(std::move(fnDecl->block));
  }
};

llvm::Function* Builder::BuildFnProto(std::shared_ptr<Decl> decl) {
  auto ty = (llvm::FunctionType*)GetLLVMTyFromTy(decl->type);
  return llvm::Function::Create(ty, llvm::GlobalValue::ExternalLinkage,
                                decl->name, module_);
}

void Builder::BuildStmt(std::unique_ptr<hir::Stmt> stmt) {
  switch (stmt->StmtKind()) {
    case hir::Stmt::EXPR:
      BuildExpr(unique_cast<hir::Stmt, hir::Expr>(std::move(stmt)));
      break;
    case hir::Stmt::RET:
      BuildRetStmt(unique_cast<hir::Stmt, hir::RetStmt>(std::move(stmt)));
      break;
    case hir::Stmt::VAR_DECL:
      BuildVarDeclStmt(
          unique_cast<hir::Stmt, hir::VarDeclStmt>(std::move(stmt)));
      break;
    case hir::Stmt::ASSIGN:
      BuildAssignStmt(unique_cast<hir::Stmt, hir::AssignStmt>(std::move(stmt)));
      break;
    case hir::Stmt::IF:
      BuildIfStmt(unique_cast<hir::Stmt, hir::IfStmt>(std::move(stmt)));
      break;
    case hir::Stmt::BLOCK:
      BuildBlock(unique_cast<hir::Stmt, hir::Block>(std::move(stmt)));
      break;
  }
}

void Builder::BuildRetStmt(std::unique_ptr<hir::RetStmt> stmt) {
  if (stmt->expr) {
    auto value = BuildExpr(std::move(stmt->expr));
    builder_.CreateRet(value);
  } else {
    builder_.CreateRetVoid();
  }
}

void Builder::BuildVarDeclStmt(std::unique_ptr<hir::VarDeclStmt> stmt) {
  auto decl = stmt->decl;
  auto value = BuildExpr(std::move(stmt->expr));
  auto ty = GetLLVMTyFromTy(decl->type);
  llvm::AllocaInst* alloca = builder_.CreateAlloca(ty, nullptr);
  builder_.CreateStore(value, alloca);
  declMap_[decl] = alloca;
}

void Builder::BuildAssignStmt(std::unique_ptr<hir::AssignStmt> stmt) {
  auto value = BuildExpr(std::move(stmt->expr));
  auto alloca = declMap_[stmt->decl];
  builder_.CreateStore(value, alloca);
}

void Builder::BuildIfStmt(std::unique_ptr<hir::IfStmt> ifStmt) {
  auto condVal = BuildExpr(std::move(ifStmt->cond));
  llvm::Value* cond =
      builder_.CreateICmpNE(condVal, llvm::ConstantInt::getFalse(ctx_));
  llvm::BasicBlock* thenBB =
      llvm::BasicBlock::Create(ctx_, "then", currentFunc_);
  llvm::BasicBlock* elseBB =
      llvm::BasicBlock::Create(ctx_, "else", currentFunc_);
  builder_.CreateCondBr(cond, thenBB, elseBB);
  builder_.SetInsertPoint(thenBB);

  BuildBlock(std::move(ifStmt->block));

  builder_.CreateBr(elseBB);
  builder_.SetInsertPoint(elseBB);

  if (ifStmt->els) {
    if (ifStmt->els->StmtKind() == hir::Stmt::Kind::IF) {
      BuildIfStmt(unique_cast<hir::Stmt, hir::IfStmt>(std::move(ifStmt->els)));
    } else if (ifStmt->els->StmtKind() == hir::Stmt::Kind::BLOCK) {
      BuildBlock(unique_cast<hir::Stmt, hir::Block>(std::move(ifStmt->els)));
    }
  }
}

void Builder::BuildBlock(std::unique_ptr<hir::Block> block) {
  while (!block->stmts.empty()) {
    auto stmt = std::move(block->stmts.front());
    block->stmts.pop_front();
    BuildStmt(std::move(stmt));
  }
}

llvm::Value* Builder::BuildBinary(std::unique_ptr<hir::Binary> binary) {
  auto ty = binary->Ty();
  auto lVal = BuildExpr(std::move(binary->lhs));
  auto rVal = BuildExpr(std::move(binary->rhs));
  llvm::Instruction::BinaryOps op;
  switch (binary->binOp) {
    case ast::BinOp::LT:
      /* op = llvm::Instruction::BinaryOps::Add; */
      break;
    case ast::BinOp::LE:
      /* op = llvm::Instruction::BinaryOps::Add; */
      break;
    case ast::BinOp::GT:
      /* op = llvm::Instruction::BinaryOps::Add; */
      break;
    case ast::BinOp::GE:
      /* op = llvm::Instruction::BinaryOps::Add; */
      break;
    case ast::BinOp::ADD:
      op = ty->IsFloat() ? llvm::Instruction::BinaryOps::FAdd
                         : llvm::Instruction::BinaryOps::Add;
      break;
    case ast::BinOp::SUB:
      op = ty->IsFloat() ? llvm::Instruction::BinaryOps::FSub
                         : llvm::Instruction::BinaryOps::Sub;
      break;
    case ast::BinOp::MUL:
      op = ty->IsFloat() ? llvm::Instruction::BinaryOps::FMul
                         : llvm::Instruction::BinaryOps::Mul;
      break;
    case ast::BinOp::DIV:
      op = ty->IsFloat() ? llvm::Instruction::BinaryOps::FDiv
                         : llvm::Instruction::BinaryOps::SDiv;
      break;
    case ast::BinOp::MOD:
      op = ty->IsFloat() ? llvm::Instruction::BinaryOps::FRem
                         : llvm::Instruction::BinaryOps::SRem;
      break;
  }

  return builder_.CreateBinOp(op, lVal, rVal);
}

llvm::Value* Builder::BuildExpr(std::unique_ptr<hir::Expr> expr) {
  switch (expr->ExprKind()) {
    case hir::Expr::Kind::BINARY: {
      return BuildBinary(unique_cast<hir::Expr, hir::Binary>(std::move(expr)));
    } break;
    case hir::Expr::Kind::VALUE: {
      auto value = (hir::Value*)expr.get();
      switch (value->ValueKind()) {
        case hir::Value::Kind::VARIABLE: {
          auto var = (hir::Variable*)value;
          auto alloc = declMap_[var->decl];
          return builder_.CreateLoad(GetLLVMTyFromTy(expr->Ty()), alloc,
                                     var->decl->name);
        } break;
        case hir::Value::Kind::CONSTANT:
          return BuildConstant(
              unique_cast<hir::Expr, hir::Constant>(std::move(expr)));
      }
    } break;
    case hir::Expr::Kind::CALL: {
      auto call = unique_cast<hir::Expr, hir::Call>(std::move(expr));
      std::vector<llvm::Value*> argValues(call->args.size());
      int i = 0;
      while (!call->args.empty()) {
        auto arg = std::move(call->args.front());
        call->args.pop_front();
        argValues[i] = BuildExpr(std::move(arg));
        i++;
      }
      auto fnType = declMap_[call->decl];
      return llvm::CallInst::Create(fnType, argValues, call->decl->name);
    } break;
    case hir::Expr::Kind::UNARY: {
      /* auto unary = (hir::Unary*)expr; */
    } break;
  }
  std::cerr << "unimplemented expr" << std::endl;
  exit(1);
}

llvm::Constant* Builder::BuildConstant(
    std::unique_ptr<hir::Constant> constant) {
  switch (constant->ConstantKind()) {
    case hir::Constant::Kind::INT: {
      auto v =
          unique_cast<hir::Constant, hir::IntConstant>(std::move(constant));
      return llvm::ConstantInt::getSigned(GetLLVMTyFromTy(v->Ty()), v->val);
    } break;
    case hir::Constant::Kind::FLOAT: {
      auto v =
          unique_cast<hir::Constant, hir::FloatConstant>(std::move(constant));
      return llvm::ConstantFP::get(GetLLVMTyFromTy(v->Ty()), v->val);
    } break;
    case hir::Constant::Kind::CHAR: {
      auto v =
          unique_cast<hir::Constant, hir::CharConstant>(std::move(constant));
      return llvm::ConstantInt::getSigned(GetLLVMTyFromTy(v->Ty()), v->val);
    } break;
    case hir::Constant::Kind::BOOL: {
      auto v =
          unique_cast<hir::Constant, hir::BoolConstant>(std::move(constant));
      return v->val ? llvm::ConstantInt::getTrue(ctx_)
                    : llvm::ConstantInt::getFalse(ctx_);
    } break;
    case hir::Constant::Kind::STRING: {
      auto v =
          unique_cast<hir::Constant, hir::StringConstant>(std::move(constant));
    } break;
  };
  // TODO:
  std::cerr << "unimplemented constant" << std::endl;
  exit(1);
  return nullptr;
}

void Builder::EmitLLVMIR(std::string filename) {
  std::error_code errCode;
  llvm::raw_fd_ostream out(filename, errCode);
  if (errCode) {
    throw std::runtime_error(errCode.message());
  }
  module_.print(out, nullptr);
}

void Builder::EmitLLVMBC(std::string filename) {
  std::error_code errCode;
  llvm::raw_fd_ostream out(filename, errCode);
  if (errCode) {
    throw std::runtime_error(errCode.message());
  }
  llvm::WriteBitcodeToFile(module_, out);
}

void Builder::EmitCodeGen(std::string filename,
                          llvm::TargetMachine::CodeGenFileType ft) {
  std::error_code errCode;
  llvm::raw_fd_ostream out(filename, errCode);
  if (errCode) {
    throw std::runtime_error(errCode.message());
  }
  llvm::legacy::PassManager pass;
  module_.setDataLayout(machine_->createDataLayout());
  machine_->addPassesToEmitFile(pass, out, nullptr, ft);
  pass.run(module_);
  out.flush();
}

void Builder::EmitASM(std::string filename) {
  EmitCodeGen(filename,
              llvm::TargetMachine::CodeGenFileType::CGFT_AssemblyFile);
}

void Builder::EmitOBJ(std::string filename) {
  EmitCodeGen(filename, llvm::TargetMachine::CodeGenFileType::CGFT_ObjectFile);
}

}  // namespace felis
