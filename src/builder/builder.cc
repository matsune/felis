#include "builder.h"

#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Verifier.h>

#include "macro.h"
#include "ptr.h"

namespace felis {

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
      return llvm::Type::getInt8PtrTy(ctx_);
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
      UNREACHABLE
  }
}

void Builder::Build(std::unique_ptr<hir::File> file) {
  for (auto& ext : file->externs) {
    declMap_[ext->decl] = BuildFnProto(ext->decl);
  }
  for (auto& fnDecl : file->fnDecls) {
    declMap_[fnDecl->decl] = BuildFnProto(fnDecl->decl);
  }

  while (!file->fnDecls.empty()) {
    auto fnDecl = std::move(file->fnDecls.front());
    file->fnDecls.pop_front();
    auto func = (llvm::Function*)declMap_[fnDecl->decl];
    currentFunc_ = func;
    auto bb = llvm::BasicBlock::Create(ctx_, "", func);
    builder_.SetInsertPoint(bb);

    auto it = func->arg_begin();
    while (!fnDecl->args.empty()) {
      auto arg = std::move(fnDecl->args.front());
      fnDecl->args.pop_front();
      llvm::AllocaInst* alloca =
          builder_.CreateAlloca(it->getType(), nullptr, arg->name);
      builder_.CreateStore(it, alloca);
      declMap_[arg] = alloca;
      it++;
    }

    BuildBlock(std::move(fnDecl->block), nullptr);

    /* std::string str; */
    /* llvm::raw_string_ostream s(str); */
    auto& s = llvm::outs();
    if (llvm::verifyFunction(*func, &s)) {
      /* throw CompileError(s.str()); */
    }
  }
};

llvm::Function* Builder::BuildFnProto(std::shared_ptr<Decl> decl) {
  auto ty = (llvm::FunctionType*)GetLLVMTyFromTy(decl->type);
  return llvm::Function::Create(ty, llvm::GlobalValue::ExternalLinkage,
                                decl->name, module_);
}

void Builder::BuildStmt(std::unique_ptr<hir::Stmt> stmt,
                        llvm::BasicBlock* afterBB) {
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
    case hir::Stmt::IF: {
      BuildIfStmt(unique_cast<hir::Stmt, hir::IfStmt>(std::move(stmt)),
                  afterBB);

    } break;
    case hir::Stmt::BLOCK:
      BuildBlock(unique_cast<hir::Stmt, hir::Block>(std::move(stmt)), afterBB);
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
  llvm::AllocaInst* alloca = builder_.CreateAlloca(ty, nullptr, decl->name);
  builder_.CreateStore(value, alloca);
  declMap_[decl] = alloca;
}

void Builder::BuildAssignStmt(std::unique_ptr<hir::AssignStmt> stmt) {
  auto value = BuildExpr(std::move(stmt->expr));
  auto alloca = declMap_[stmt->decl];
  builder_.CreateStore(value, alloca);
}

void Builder::BuildIfStmt(std::unique_ptr<hir::IfStmt> ifStmt,
                          llvm::BasicBlock* endBB) {
  bool hasElse = ifStmt->els != nullptr;
  auto condVal = BuildExpr(std::move(ifStmt->cond));
  llvm::BasicBlock* thenBB =
      llvm::BasicBlock::Create(ctx_, "then", currentFunc_, endBB);

  if (hasElse) {
    //
    // if cond {
    //    <- thenBB
    // } else {
    //    <- elseBB
    // }
    // <- endBB
    //
    llvm::BasicBlock* elseBB =
        llvm::BasicBlock::Create(ctx_, "else", currentFunc_, endBB);
    builder_.CreateCondBr(condVal, thenBB, elseBB);
    builder_.SetInsertPoint(thenBB);
    BuildBlock(std::move(ifStmt->block), endBB);

    builder_.SetInsertPoint(elseBB);
    if (ifStmt->els->StmtKind() == hir::Stmt::Kind::IF) {
      BuildIfStmt(unique_cast<hir::Stmt, hir::IfStmt>(std::move(ifStmt->els)),
                  endBB);
    } else if (ifStmt->els->StmtKind() == hir::Stmt::Kind::BLOCK) {
      BuildBlock(unique_cast<hir::Stmt, hir::Block>(std::move(ifStmt->els)),
                 endBB);
    }

  } else {
    // No else if-statement
    //
    // if cond {
    //    <- thenBB
    // }
    // <- endBB
    //
    builder_.CreateCondBr(condVal, thenBB, endBB);
    builder_.SetInsertPoint(thenBB);
    BuildBlock(std::move(ifStmt->block), endBB);
  }
}

void Builder::BuildBlock(std::unique_ptr<hir::Block> block,
                         llvm::BasicBlock* afterBB) {
  while (!block->stmts.empty()) {
    auto stmt = std::move(block->stmts.front());
    block->stmts.pop_front();

    bool isLast = block->stmts.empty();
    if (!isLast && stmt->StmtKind() == hir::Stmt::Kind::IF) {
      auto endBB = llvm::BasicBlock::Create(ctx_, "end", currentFunc_, afterBB);
      BuildStmt(std::move(stmt), endBB);
      builder_.SetInsertPoint(endBB);
    } else {
      BuildStmt(std::move(stmt), afterBB);
    }
  }
  if (!builder_.GetInsertBlock()->getTerminator()) {
    builder_.CreateBr(afterBB);
  }
}

llvm::Value* Builder::BuildBinary(std::unique_ptr<hir::Binary> binary) {
  auto ty = binary->Ty();
  bool isF = binary->lhs->Ty()->IsFloat();
  auto lVal = BuildExpr(std::move(binary->lhs));
  auto rVal = BuildExpr(std::move(binary->rhs));
  switch (binary->op) {
    case hir::Binary::Op::LT: {
      if (isF) {
        return builder_.CreateFCmpOLT(lVal, rVal);
      } else {
        return builder_.CreateICmpSLT(lVal, rVal);
      }
    }
    case hir::Binary::Op::LE: {
      if (isF) {
        return builder_.CreateFCmpOLE(lVal, rVal);
      } else {
        return builder_.CreateICmpSLE(lVal, rVal);
      }
    }

    case hir::Binary::Op::GT: {
      if (isF) {
        return builder_.CreateFCmpOGT(lVal, rVal);
      } else {
        return builder_.CreateICmpSGT(lVal, rVal);
      }
    }
    case hir::Binary::Op::GE: {
      if (isF) {
        return builder_.CreateFCmpOGE(lVal, rVal);
      } else {
        return builder_.CreateICmpSGE(lVal, rVal);
      }
    }
    case hir::Binary::Op::ADD: {
      if (isF) {
        return builder_.CreateFAdd(lVal, rVal);
      } else {
        return builder_.CreateAdd(lVal, rVal);
      }
    }
    case hir::Binary::Op::SUB: {
      if (isF) {
        return builder_.CreateFSub(lVal, rVal);
      } else {
        return builder_.CreateSub(lVal, rVal);
      }
    }
    case hir::Binary::Op::MUL: {
      if (isF) {
        return builder_.CreateFMul(lVal, rVal);
      } else {
        return builder_.CreateMul(lVal, rVal);
      }
    }
    case hir::Binary::Op::DIV: {
      if (isF) {
        return builder_.CreateFDiv(lVal, rVal);
      } else {
        return builder_.CreateSDiv(lVal, rVal);
      }
    }
    case hir::Binary::Op::MOD: {
      if (isF) {
        return builder_.CreateFRem(lVal, rVal);
      } else {
        return builder_.CreateSRem(lVal, rVal);
      }
    }
  }
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
          auto variable = (hir::Variable*)value;
          auto var = declMap_[variable->decl];
          return builder_.CreateLoad(GetLLVMTyFromTy(variable->Ty()), var);

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
        auto expr = BuildExpr(std::move(arg));
        argValues[i] = expr;
        i++;
      }
      bool isVoid = call->decl->AsFuncType()->ret->IsVoid();
      auto fnType = (llvm::Function*)declMap_[call->decl];
      return builder_.CreateCall(fnType, argValues,
                                 isVoid ? "" : call->decl->name);
    } break;
    case hir::Expr::Kind::UNARY: {
      /* auto unary = (hir::Unary*)expr; */
    } break;
  }
  UNREACHABLE
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
      return builder_.CreateGlobalStringPtr(v->val);
    } break;
  };
  UNREACHABLE
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
  if (machine_->addPassesToEmitFile(pass, out, nullptr, ft)) {
    throw std::runtime_error("TargetMachine can't emit a file of this type");
  }
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
