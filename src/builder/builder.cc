#include "builder.h"

#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Verifier.h>

#include "macro.h"
#include "unique.h"

namespace felis {

llvm::Type* Builder::GetLLVMTyFromTy(std::shared_ptr<Type> ty) {
  switch (ty->TypeKind()) {
    case Type::Kind::BOOL:
      return llvm::Type::getInt1Ty(ctx_);
    /* case Type::Kind::CHAR: */
    /*   return llvm::Type::getInt32Ty(ctx_); */
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
      auto func_type = (FuncType*)ty.get();
      std::vector<llvm::Type*> args;
      for (auto& arg : func_type->args) {
        args.push_back(GetLLVMTyFromTy(arg));
      }
      return llvm::FunctionType::get(GetLLVMTyFromTy(func_type->ret), args,
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
    decl_map_[ext->decl] = BuildFnProto(ext->decl);
  }
  for (auto& fn_decl : file->fn_decls) {
    decl_map_[fn_decl->decl] = BuildFnProto(fn_decl->decl);
  }

  while (!file->fn_decls.empty()) {
    auto fn_decl = file->fn_decls.move_front();
    auto func = (llvm::Function*)decl_map_[fn_decl->decl];
    current_func_ = func;
    auto bb = llvm::BasicBlock::Create(ctx_, "", func);
    builder_.SetInsertPoint(bb);

    auto it = func->arg_begin();
    while (!fn_decl->args.empty()) {
      auto arg = fn_decl->args.front();
      fn_decl->args.pop_front();
      llvm::AllocaInst* alloca =
          builder_.CreateAlloca(it->getType(), nullptr, arg->name);
      builder_.CreateStore(it, alloca);
      decl_map_[arg] = alloca;
      it++;
    }

    BuildBlock(std::move(fn_decl->block), nullptr);

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
                        llvm::BasicBlock* after_bb) {
  switch (stmt->StmtKind()) {
    case hir::Stmt::EXPR:
      BuildExpr(unique_cast<hir::Expr>(std::move(stmt)));
      break;
    case hir::Stmt::RET:
      BuildRetStmt(unique_cast<hir::RetStmt>(std::move(stmt)));
      break;
    case hir::Stmt::VAR_DECL:
      BuildVarDeclStmt(unique_cast<hir::VarDeclStmt>(std::move(stmt)));
      break;
    case hir::Stmt::ASSIGN:
      BuildAssignStmt(unique_cast<hir::AssignStmt>(std::move(stmt)));
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
  decl_map_[decl] = alloca;
}

void Builder::BuildAssignStmt(std::unique_ptr<hir::AssignStmt> stmt) {
  auto value = BuildExpr(std::move(stmt->expr));
  auto alloca = decl_map_[stmt->decl];
  builder_.CreateStore(value, alloca);
}

void Builder::BuildIf(std::unique_ptr<hir::If> if_stmt,
                      llvm::BasicBlock* end_bb) {
  bool has_else = if_stmt->HasElse();
  auto cond_val = BuildExpr(std::move(if_stmt->cond));
  llvm::BasicBlock* then_bb =
      llvm::BasicBlock::Create(ctx_, "then", current_func_, end_bb);

  if (has_else) {
    //
    // if cond {
    //    <- then_bb
    // } else {
    //    <- else_bb
    // }
    // <- end_bb
    //
    llvm::BasicBlock* else_bb =
        llvm::BasicBlock::Create(ctx_, "else", current_func_, end_bb);
    builder_.CreateCondBr(cond_val, then_bb, else_bb);
    builder_.SetInsertPoint(then_bb);
    BuildBlock(std::move(if_stmt->block), end_bb);

    builder_.SetInsertPoint(else_bb);
    if (if_stmt->IsElseIf()) {
      BuildIf(unique_cast<hir::If>(std::move(if_stmt->els)), end_bb);
    } else if (if_stmt->IsElseBlock()) {
      BuildBlock(unique_cast<hir::Block>(std::move(if_stmt->els)), end_bb);
    }

  } else {
    // No else if-statement
    //
    // if cond {
    //    <- then_bb
    // }
    // <- end_bb
    //
    builder_.CreateCondBr(cond_val, then_bb, end_bb);
    builder_.SetInsertPoint(then_bb);
    BuildBlock(std::move(if_stmt->block), end_bb);
  }
}

void Builder::BuildBlock(std::unique_ptr<hir::Block> block,
                         llvm::BasicBlock* after_bb) {
  while (!block->stmts.empty()) {
    auto stmt = block->stmts.move_front();

    bool is_last = block->stmts.empty();
    // TODO
    UNIMPLEMENTED
    /* if (!is_last && stmt->ExprKind() == hir::Expr::Kind::IF) { */
    /*   auto end_bb = */
    /*       llvm::BasicBlock::Create(ctx_, "end", current_func_, after_bb); */
    /*   BuildStmt(std::move(stmt), end_bb); */
    /*   builder_.SetInsertPoint(end_bb); */
    /* } else { */
    /*   BuildStmt(std::move(stmt), after_bb); */
    /* } */
  }
  if (!builder_.GetInsertBlock()->getTerminator()) {
    builder_.CreateBr(after_bb);
  }
}

llvm::Value* Builder::BuildBinary(std::unique_ptr<hir::Binary> binary) {
  auto ty = binary->Ty();
  bool is_f = binary->lhs->Ty()->IsTypedFloat();
  auto l_val = BuildExpr(std::move(binary->lhs));
  auto r_val = BuildExpr(std::move(binary->rhs));
  switch (binary->op) {
    case hir::Binary::Op::LT: {
      if (is_f) {
        return builder_.CreateFCmpOLT(l_val, r_val);
      } else {
        return builder_.CreateICmpSLT(l_val, r_val);
      }
    }
    case hir::Binary::Op::LE: {
      if (is_f) {
        return builder_.CreateFCmpOLE(l_val, r_val);
      } else {
        return builder_.CreateICmpSLE(l_val, r_val);
      }
    }

    case hir::Binary::Op::GT: {
      if (is_f) {
        return builder_.CreateFCmpOGT(l_val, r_val);
      } else {
        return builder_.CreateICmpSGT(l_val, r_val);
      }
    }
    case hir::Binary::Op::GE: {
      if (is_f) {
        return builder_.CreateFCmpOGE(l_val, r_val);
      } else {
        return builder_.CreateICmpSGE(l_val, r_val);
      }
    }
    case hir::Binary::Op::ADD: {
      if (is_f) {
        return builder_.CreateFAdd(l_val, r_val);
      } else {
        return builder_.CreateAdd(l_val, r_val);
      }
    }
    case hir::Binary::Op::SUB: {
      if (is_f) {
        return builder_.CreateFSub(l_val, r_val);
      } else {
        return builder_.CreateSub(l_val, r_val);
      }
    }
    case hir::Binary::Op::MUL: {
      if (is_f) {
        return builder_.CreateFMul(l_val, r_val);
      } else {
        return builder_.CreateMul(l_val, r_val);
      }
    }
    case hir::Binary::Op::DIV: {
      if (is_f) {
        return builder_.CreateFDiv(l_val, r_val);
      } else {
        return builder_.CreateSDiv(l_val, r_val);
      }
    }
    case hir::Binary::Op::MOD: {
      if (is_f) {
        return builder_.CreateFRem(l_val, r_val);
      } else {
        return builder_.CreateSRem(l_val, r_val);
      }
    }
  }
}

llvm::Value* Builder::BuildExpr(std::unique_ptr<hir::Expr> expr) {
  switch (expr->ExprKind()) {
    case hir::Expr::Kind::BINARY: {
      return BuildBinary(unique_cast<hir::Binary>(std::move(expr)));
    } break;
    case hir::Expr::Kind::VALUE: {
      auto value = (hir::Value*)expr.get();
      switch (value->ValueKind()) {
        case hir::Value::Kind::VARIABLE: {
          auto variable = (hir::Variable*)value;
          auto var = decl_map_[variable->decl];
          return builder_.CreateLoad(GetLLVMTyFromTy(variable->Ty()), var);

        } break;
        case hir::Value::Kind::CONSTANT:
          return BuildConstant(unique_cast<hir::Constant>(std::move(expr)));
      }
    } break;
    case hir::Expr::Kind::CALL: {
      auto call = unique_cast<hir::Call>(std::move(expr));
      std::vector<llvm::Value*> arg_values(call->args.size());
      int i = 0;
      while (!call->args.empty()) {
        auto arg = call->args.move_front();
        auto expr = BuildExpr(std::move(arg));
        arg_values[i] = expr;
        i++;
      }
      bool is_void = call->decl->AsFuncType()->ret->IsVoid();
      auto fn_type = (llvm::Function*)decl_map_[call->decl];
      return builder_.CreateCall(fn_type, arg_values,
                                 is_void ? "" : call->decl->name);
    } break;
    case hir::Expr::Kind::UNARY: {
      /* auto unary = (hir::Unary*)expr; */
    } break;
    case hir::Expr::IF: {
      UNIMPLEMENTED
      /* BuildIf(unique_cast< hir::If>(std::move(stmt)), after_bb); */

    } break;
    case hir::Expr::BLOCK:
      UNIMPLEMENTED
      /* BuildBlock(unique_cast< hir::Block>(std::move(stmt)),
       * after_bb); */
      break;
  }
  UNREACHABLE
}

llvm::Constant* Builder::BuildConstant(
    std::unique_ptr<hir::Constant> constant) {
  switch (constant->ConstantKind()) {
    case hir::Constant::Kind::INT: {
      auto v = unique_cast<hir::IntConstant>(std::move(constant));
      return llvm::ConstantInt::getSigned(GetLLVMTyFromTy(v->Ty()), v->val);
    } break;
    case hir::Constant::Kind::FLOAT: {
      auto v = unique_cast<hir::FloatConstant>(std::move(constant));
      return llvm::ConstantFP::get(GetLLVMTyFromTy(v->Ty()), v->val);
    } break;
    /* case hir::Constant::Kind::CHAR: { */
    /*   auto v = unique_cast<hir::CharConstant>(std::move(constant)); */
    /*   return llvm::ConstantInt::getSigned(GetLLVMTyFromTy(v->Ty()), v->val);
     */
    /* } break; */
    case hir::Constant::Kind::BOOL: {
      auto v = unique_cast<hir::BoolConstant>(std::move(constant));
      return v->val ? llvm::ConstantInt::getTrue(ctx_)
                    : llvm::ConstantInt::getFalse(ctx_);
    } break;
    case hir::Constant::Kind::STRING: {
      auto v = unique_cast<hir::StringConstant>(std::move(constant));
      return builder_.CreateGlobalStringPtr(v->val);
    } break;
  };
  UNREACHABLE
}

void Builder::EmitLLVMIR(std::string filename) {
  std::error_code err_code;
  llvm::raw_fd_ostream out(filename, err_code);
  if (err_code) {
    throw std::runtime_error(err_code.message());
  }
  module_.print(out, nullptr);
}

void Builder::EmitLLVMBC(std::string filename) {
  std::error_code err_code;
  llvm::raw_fd_ostream out(filename, err_code);
  if (err_code) {
    throw std::runtime_error(err_code.message());
  }
  llvm::WriteBitcodeToFile(module_, out);
}

void Builder::EmitCodeGen(std::string filename,
                          llvm::TargetMachine::CodeGenFileType ft) {
  std::error_code err_code;
  llvm::raw_fd_ostream out(filename, err_code);
  if (err_code) {
    throw std::runtime_error(err_code.message());
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
