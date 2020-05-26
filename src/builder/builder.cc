#include "builder.h"

#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Verifier.h>

#include "macro.h"
#include "unique.h"

namespace felis {

llvm::Type* Builder::LLVMType(const std::shared_ptr<Type>& ty) {
  if (ty->IsBool()) return llvm::Type::getInt1Ty(ctx_);
  if (ty->IsI8()) return llvm::Type::getInt8Ty(ctx_);
  if (ty->IsI16()) return llvm::Type::getInt16Ty(ctx_);
  if (ty->IsI32()) return llvm::Type::getInt32Ty(ctx_);
  if (ty->IsI64()) return llvm::Type::getInt64Ty(ctx_);
  if (ty->IsF32()) return llvm::Type::getFloatTy(ctx_);
  if (ty->IsF64()) return llvm::Type::getDoubleTy(ctx_);
  if (ty->IsString()) return llvm::Type::getInt8PtrTy(ctx_);
  if (ty->IsFunc()) {
    auto func_type = std::dynamic_pointer_cast<FuncType>(ty);
    std::vector<llvm::Type*> args;
    for (auto& arg : func_type->args) {
      args.push_back(LLVMType(std::dynamic_pointer_cast<FixedType>(arg)));
    }
    return llvm::FunctionType::get(
        LLVMType(std::dynamic_pointer_cast<FixedType>(func_type->ret)), args,
        false);
  }
  if (ty->IsVoid()) return llvm::Type::getVoidTy(ctx_);

  std::cout << ToString(ty) << std::endl;
  UNREACHABLE
}

void Builder::Build(std::unique_ptr<hir::File> file) {
  while (!file->externs.empty()) {
    auto ext = file->externs.move_front();
    auto func_value = BuildFnProto(ext->decl);
    RecordValue(ext->decl, func_value);
  }
  for (auto& fn_decl : file->fn_decls) {
    auto func_value = BuildFnProto(fn_decl->decl);
    RecordValue(fn_decl->decl, func_value);
  }

  while (!file->fn_decls.empty()) {
    auto fn_decl = file->fn_decls.move_front();
    auto func = static_cast<llvm::Function*>(GetValue(fn_decl->decl));
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
      RecordValue(arg, alloca);
      it++;
    }

    if (fn_decl->block->stmts.empty()) {
      builder_.CreateRetVoid();
    }

    int i = 0;
    while (!fn_decl->block->stmts.empty()) {
      std::cout << "BuildStmt " << i++ << std::endl;
      BuildStmt(fn_decl->block->stmts.move_front());
    }

    /* std::string str; */
    /* llvm::raw_string_ostream s(str); */
    auto& s = llvm::outs();
    if (llvm::verifyFunction(*func, &s)) {
      return;
      /* throw CompileError(s.str()); */
    }
  }
};

llvm::Function* Builder::BuildFnProto(std::shared_ptr<Decl>& decl) {
  auto ty = (llvm::FunctionType*)LLVMType(decl->type);
  return llvm::Function::Create(ty, llvm::GlobalValue::ExternalLinkage,
                                decl->name, module_);
}

llvm::Value* Builder::BuildStmt(std::unique_ptr<hir::Stmt> stmt) {
  switch (stmt->StmtKind()) {
    case hir::Stmt::EXPR:
      return BuildExpr(unique_cast<hir::Expr>(std::move(stmt)));
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
  return nullptr;
}

void Builder::BuildRetStmt(std::unique_ptr<hir::RetStmt> stmt) {
  if (stmt->expr) {
    auto expr = BuildExpr(std::move(stmt->expr));
    builder_.CreateRet(expr);
  } else {
    builder_.CreateRetVoid();
  }
}

void Builder::BuildVarDeclStmt(std::unique_ptr<hir::VarDeclStmt> stmt) {
  auto decl = stmt->decl;
  auto value = BuildExpr(std::move(stmt->expr));
  auto ty = LLVMType(decl->type);
  llvm::AllocaInst* alloca = builder_.CreateAlloca(ty, nullptr, decl->name);
  builder_.CreateStore(value, alloca);
  RecordValue(decl, alloca);
}

void Builder::BuildAssignStmt(std::unique_ptr<hir::AssignStmt> stmt) {
  auto value = BuildExpr(std::move(stmt->expr));
  auto alloca = GetValue(stmt->decl);
  builder_.CreateStore(value, alloca);
}

llvm::Value* Builder::BuildBinary(std::unique_ptr<hir::Binary> binary) {
  auto ty = binary->Type();
  auto is_float = binary->lhs->Type()->IsFixedFloat();
  auto lhs = BuildExpr(std::move(binary->lhs));
  auto rhs = BuildExpr(std::move(binary->rhs));

  llvm::Instruction::BinaryOps bin_op;
  switch (binary->op) {
    case hir::Binary::Op::EQEQ:
      return is_float ? builder_.CreateFCmpOEQ(lhs, rhs)
                      : builder_.CreateICmpEQ(lhs, rhs);
    case hir::Binary::Op::NEQ:
      return is_float ? builder_.CreateFCmpONE(lhs, rhs)
                      : builder_.CreateICmpNE(lhs, rhs);
    case hir::Binary::Op::LT:
      return is_float ? builder_.CreateFCmpOLT(lhs, rhs)
                      : builder_.CreateICmpSLT(lhs, rhs);
    case hir::Binary::Op::LE:
      return is_float ? builder_.CreateFCmpOLE(lhs, rhs)
                      : builder_.CreateICmpSLE(lhs, rhs);
    case hir::Binary::Op::GT:
      return is_float ? builder_.CreateFCmpOGT(lhs, rhs)
                      : builder_.CreateICmpSGT(lhs, rhs);
    case hir::Binary::Op::GE:
      return is_float ? builder_.CreateFCmpOGE(lhs, rhs)
                      : builder_.CreateICmpSGE(lhs, rhs);
    case hir::Binary::Op::ADD:
      return is_float ? builder_.CreateFAdd(lhs, rhs)
                      : builder_.CreateAdd(lhs, rhs);
    case hir::Binary::Op::SUB:
      return is_float ? builder_.CreateFSub(lhs, rhs)
                      : builder_.CreateSub(lhs, rhs);
    case hir::Binary::Op::MUL:
      return is_float ? builder_.CreateFMul(lhs, rhs)
                      : builder_.CreateMul(lhs, rhs);
    case hir::Binary::Op::DIV:
      return is_float ? builder_.CreateFDiv(lhs, rhs)
                      : builder_.CreateSDiv(lhs, rhs);
    case hir::Binary::Op::MOD:
      assert(!is_float);
      return builder_.CreateSRem(lhs, rhs);
  }
}

void Builder::BuildBlock(std::unique_ptr<hir::Block> block,
                         llvm::AllocaInst* into, llvm::BasicBlock* after_bb) {
  std::cout << "BuildBlock " << block.get() << " into: " << into << " after_bb "
            << after_bb << std::endl;
  llvm::Value* value;
  while (!block->stmts.empty()) {
    auto stmt = block->stmts.move_front();
    value = BuildStmt(std::move(stmt));
  }

  if (value && into) {
    builder_.CreateStore(value, into);
  }

  if (!builder_.GetInsertBlock()->getTerminator()) {
    builder_.CreateBr(after_bb);
    /* builder_.SetInsertPoint(after_bb); */
  }
}

void Builder::BuildIf(std::unique_ptr<hir::If> if_stmt, llvm::AllocaInst* into,
                      llvm::BasicBlock* after_bb) {
  std::cout << "BuildIf " << if_stmt.get() << " into: " << into << " after_bb"
            << after_bb << std::endl;
  auto has_else = if_stmt->HasElse();
  auto cond_val = BuildExpr(std::move(if_stmt->cond));
  llvm::BasicBlock* then_bb =
      llvm::BasicBlock::Create(ctx_, "then", current_func_, after_bb);

  if (has_else) {
    llvm::BasicBlock* else_bb =
        llvm::BasicBlock::Create(ctx_, "else", current_func_, after_bb);
    builder_.CreateCondBr(cond_val, then_bb, else_bb);
    builder_.SetInsertPoint(then_bb);
    //   br cond_val, then_bb, after_bb
    // then_bb:
    //   <- InsertPoint
    //
    // else_bb:
    //
    // after_bb:
    BuildBlock(std::move(if_stmt->block), into, after_bb);

    builder_.SetInsertPoint(else_bb);
    if (if_stmt->IfElseIf()) {
      BuildIf(unique_cast<hir::If>(std::move(if_stmt->els)), into, after_bb);
    } else {
      BuildBlock(unique_cast<hir::Block>(std::move(if_stmt->els)), into,
                 after_bb);
    }
  } else {
    builder_.CreateCondBr(cond_val, then_bb, after_bb);
    builder_.SetInsertPoint(then_bb);
    //   br cond_val, then_bb, after_bb
    // then_bb:
    //   <- InsertPoint
    //
    // after_bb:
    BuildBlock(std::move(if_stmt->block), into, after_bb);
  }
}

llvm::Value* Builder::BuildExpr(std::unique_ptr<hir::Expr> expr) {
  switch (expr->ExprKind()) {
    case hir::Expr::Kind::BINARY:
      return BuildBinary(unique_cast<hir::Binary>(std::move(expr)));

    case hir::Expr::Kind::VALUE: {
      auto value = unique_cast<hir::Value>(std::move(expr));
      switch (value->ValueKind()) {
        case hir::Value::Kind::VARIABLE: {
          auto variable = unique_cast<hir::Variable>(std::move(value));
          auto var_alloca = GetValue(variable->decl);
          return builder_.CreateLoad(LLVMType(variable->Type()), var_alloca);
        } break;
        case hir::Value::Kind::CONSTANT: {
          return BuildConstant(unique_cast<hir::Constant>(std::move(value)));
        } break;
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
      auto fn_type = GetValue(call->decl);
      return builder_.CreateCall(fn_type, arg_values);
    } break;
    case hir::Expr::Kind::UNARY: {
      auto unary = unique_cast<hir::Unary>(std::move(expr));
      bool is_float = unary->Type()->IsFixedFloat();
      auto expr = BuildExpr(std::move(unary->expr));
      switch (unary->op) {
        case hir::Unary::Op::NEG:
          return is_float
                     ? builder_.CreateFMul(
                           llvm::ConstantFP::get(expr->getType(), -1), expr)

                     : builder_.CreateMul(
                           llvm::ConstantInt::getSigned(expr->getType(), -1),
                           expr);
        case hir::Unary::Op::NOT:
          return builder_.CreateXor(expr, llvm::ConstantInt::getTrue(ctx_));
      }
    } break;
    case hir::Expr::IF:
    case hir::Expr::BLOCK: {
      auto expr_type = LLVMType(expr->Type());
      auto as_expr = !expr_type->isVoidTy();
      std::cout << "If or block stmt as_expr " << as_expr << std::endl;

      llvm::AllocaInst* alloca = nullptr;
      if (as_expr) {
        alloca = builder_.CreateAlloca(expr_type);
      }

      llvm::BasicBlock* after_bb = nullptr;
      if (as_expr || !expr->IsTerminating()) {
        after_bb =
            llvm::BasicBlock::Create(ctx_, "end", current_func_,
                                     builder_.GetInsertBlock()->getNextNode());
      }
      if (expr->ExprKind() == hir::Expr::Kind::IF) {
        BuildIf(unique_cast<hir::If>(std::move(expr)), alloca, after_bb);
      } else {
        BuildBlock(unique_cast<hir::Block>(std::move(expr)), alloca, after_bb);
      }
      if (after_bb) builder_.SetInsertPoint(after_bb);

      if (as_expr) {
        return builder_.CreateLoad(expr_type, alloca);
      } else {
        return nullptr;
      }

    } break;
  }
}

llvm::Constant* Builder::BuildConstant(
    std::unique_ptr<hir::Constant> constant) {
  switch (constant->ConstantKind()) {
    case hir::Constant::Kind::INT: {
      auto v = unique_cast<hir::IntConstant>(std::move(constant));
      return llvm::ConstantInt::getSigned(LLVMType(v->Type()), v->val);
    } break;
    case hir::Constant::Kind::FLOAT: {
      auto v = unique_cast<hir::FloatConstant>(std::move(constant));
      return llvm::ConstantFP::get(LLVMType(v->Type()), v->val);
    } break;
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
