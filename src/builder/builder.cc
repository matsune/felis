#include "builder.h"

#include <llvm/ADT/StringMap.h>
#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/MC/SubtargetFeature.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>

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

/* void Builder::Build(Expr* expr, llvm::Value*& value, Ty& ty) { */
/*   switch (expr->ExprKind()) { */
/*     case Expr::Kind::IDENT: { */
/*       auto ident = (Ident*)expr; */
/*       auto def = sm_.LookupVariable(ident->sval); */
/*       if (def == nullptr) { */
/*         throw CompileError::BuildPosFmt(expr->GetPos(), "undefined %s", */
/*                                          ident->sval.c_str()); */
/*       } */
/*       ty = def->ty; */
/*       value = def->Value(); */
/*       if (value->getType()->isPointerTy()) { */
/*         value = builder_.BuildLoad(getLLVMTyFromTy(ty), value); */
/*       } */
/*     } break; */
/*     case Expr::Kind::BINARY: { */
/*       auto binary = (BinaryExpr*)expr; */
/*       Ty lhsTy; */
/*       llvm::Value* lhsValue; */
/*       Build(binary->lhs.get(), lhsValue, lhsTy); */

/*       Ty rhsTy; */
/*       llvm::Value* rhsValue; */
/*       Build(binary->rhs.get(), rhsValue, rhsTy); */

/*       if (lhsTy != rhsTy) { */
/*         // TODO: support binary between another types */
/*         throw CompileError::BuildPosFmt( */
/*             binary->GetPos(), "another types binary expr (lhs: %s, rhs: %s)",
 */
/*             ToString(lhsTy).c_str(), ToString(rhsTy).c_str()); */
/*       } */

/*       switch (lhsTy) { */
/*         case Ty::INT: */
/*         case Ty::CHAR: */
/*         case Ty::FLOAT: */
/*           if (binary->op == BinOp::GE || binary->op == BinOp::GT || */
/*               binary->op == BinOp::LE || binary->op == BinOp::LT) { */
/*             ty = Ty::BOOL; */
/*           } else { */
/*             ty = lhsTy; */
/*           } */
/*           break; */
/*         case Ty::STRING: */
/*         case Ty::BOOL:  // TODO: != == */
/*         case Ty::VOID: */
/*         case Ty::UNKNOWN: */
/*           throw CompileError::BuildPosFmt(binary->GetPos(), */
/*                                            "unsupported binary expr type");
 */
/*       } */
/*       switch (binary->op) { */
/*         case BinOp::ADD: */
/*           value = builder_.BuildAdd(lhsValue, rhsValue); */
/*           break; */
/*         case BinOp::SUB: */
/*           value = builder_.BuildSub(lhsValue, rhsValue); */
/*           break; */
/*         case BinOp::MUL: */
/*           value = builder_.BuildMul(lhsValue, rhsValue); */
/*           break; */
/*         case BinOp::DIV: */
/*           // FIXME: */
/*           value = builder_.BuildFDiv(lhsValue, rhsValue); */
/*           break; */
/*         /1* case BinOp::MOD: *1/ */
/*         /1* value = builder_.Build//(lhsValue, rhsValue); *1/ */
/*         /1* exit(1); *1/ */
/*         /1* break; *1/ */
/*         default: */
/*           std::cout << "UNIMPLEMENTED" << std::endl; */
/*           exit(1); */
/*       } */

/*     } break; */
/*     case Expr::Kind::LIT: { */
/*       auto lit = (Lit*)expr; */
/*       switch (lit->LitKind()) { */
/*         case Lit::Kind::INT: { */
/*           auto litInt = (LitInt*)expr; */
/*           ty = Ty::INT; */
/*           value = */
/*               llvm::ConstantInt::getSigned(getLLVMTyFromTy(ty),
 * litInt->ival); */
/*         } break; */
/*         case Lit::Kind::FLOAT: { */
/*           auto litFloat = (LitFloat*)expr; */
/*           ty = Ty::FLOAT; */
/*           value = llvm::ConstantFP::get(getLLVMTyFromTy(ty), litFloat->fval);
 */
/*         } break; */
/*         case Lit::Kind::BOOL: { */
/*           auto litBool = (LitBool*)expr; */
/*           ty = Ty::BOOL; */
/*           value = litBool->bval ? llvm::ConstantInt::getTrue(ctx_) */
/*                                 : llvm::ConstantInt::getFalse(ctx_); */
/*         } break; */
/*         case Lit::Kind::CHAR: { */
/*           auto litChar = (LitChar*)expr; */
/*           ty = Ty::CHAR; */
/*           value = llvm::ConstantInt::get(getLLVMTyFromTy(ty), */
/*                                          litChar->cval.scalar, false); */
/*         } break; */
/*         case Lit::Kind::STR: { */
/*           // TODO */
/*           ty = Ty::STRING; */
/*           std::cout << "UNIMPLEMENTED STRING" << std::endl; */
/*           exit(1); */
/*         } break; */
/*       } */
/*     } break; */
/*     case Expr::Kind::CALL: { */
/*       auto call = (CallExpr*)expr; */
/*       auto def = sm_.LookupFn(call->ident->sval); */
/*       if (def == nullptr) { */
/*         throw CompileError::BuildPosFmt( */
/*             call->GetPos(), "undefined function %s",
 * call->ident->sval.c_str()); */
/*       } */
/*       if (def->args.size() != call->args.size()) { */
/*         throw CompileError::BuildPosFmt(call->GetPos(), */
/*                                          "args count doesn't match"); */
/*       } */
/*       std::vector<llvm::Value*> argValues(call->args.size()); */
/*       std::vector<Ty> argTys(call->args.size()); */
/*       for (int i = 0; i < call->args.size(); i++) { */
/*         auto& arg = call->args[i]; */
/*         Build(arg.get(), argValues[i], argTys[i]); */
/*         if (def->args[i] != argTys[i]) { */
/*           throw CompileError::BuildPosFmt(arg->GetPos(), */
/*                                            "arg type doesn't match"); */
/*         } */
/*       } */
/*       ty = def->ret; */
/*       value = llvm::CallInst::Build(def->func, argValues); */
/*     } break; */
/*     case Expr::Kind::UNARY: { */
/*       auto unary = (UnaryExpr*)expr; */
/*       Ty exprTy; */
/*       llvm::Value* exprValue; */
/*       Build(unary->expr.get(), exprValue, exprTy); */
/*       switch (unary->unOp) { */
/*         case UnOp::NEG: */
/*           if (exprTy == Ty::INT) { */
/*             ty = Ty::INT; */
/*             value = builder_.BuildNeg(exprValue); */
/*           } else if (exprTy == Ty::FLOAT) { */
/*             ty = Ty::FLOAT; */
/*             value = builder_.BuildNeg(exprValue); */
/*           } */
/*           break; */
/*         case UnOp::NOT: */
/*           if (exprTy == Ty::BOOL) { */
/*             ty = Ty::BOOL; */
/*             value = builder_.BuildNot(exprValue); */
/*           } */
/*           break; */
/*       } */
/*     } break; */
/*   } */
/* } */

/* void Builder::Build(std::unique_ptr<Stmt>& stmt) { */
/*   switch (stmt->StmtKind()) { */
/*     case Stmt::Kind::EXPR: { */
/*       auto expr = (Expr*)stmt.get(); */
/*       llvm::Value* value; */
/*       Ty ty; */
/*       Build(expr, value, ty); */
/*     } break; */

/*     case Stmt::Kind::RET: { */
/*       auto ret = (RetStmt*)stmt.get(); */
/*       llvm::Value* value(nullptr); */
/*       Ty ty; */

/*       if (ret->expr) { */
/*         Build(ret->expr.get(), value, ty); */
/*       } else { */
/*         ty = Ty::VOID; */
/*       } */

/*       if (ty == Ty::UNKNOWN) { */
/*         throw CompileError::BuildPosFmt(ret->expr->GetPos(), */
/*                                          "unknown ret type"); */
/*       } */
/*       if (currentFn_->ret != ty) { */
/*         throw CompileError::BuildPosFmt( */
/*             stmt->GetPos(), "cannot use type %s for function ret type %s", */
/*             ToString(ty).c_str(), ToString(currentFn_->ret).c_str()); */
/*       } */
/*       if (ty == Ty::VOID) { */
/*         builder_.BuildRetVoid(); */
/*       } else { */
/*         builder_.BuildRet(value); */
/*       } */
/*     } break; */

/*     case Stmt::Kind::VAR_DECL: { */
/*       auto decl = (VarDeclStmt*)stmt.get(); */
/*       std::string name = decl->name->sval; */
/*       if (!sm_.CanDeclareVariable(name)) { */
/*         throw CompileError::BuildPosFmt(decl->GetPos(), "redeclared var %s",
 */
/*                                          name.c_str()); */
/*       } */
/*       Ty ty; */
/*       llvm::Value* value; */
/*       Build(decl->expr.get(), value, ty); */

/*       llvm::AllocaInst* alloca = */
/*           builder_.BuildAlloca(getLLVMTyFromTy(ty), nullptr, name); */
/*       builder_.BuildStore(value, alloca); */

/*       auto def = std::make_shared<DefVar>(name, ty); */
/*       def->value = alloca; */
/*       sm_.InsertVariable(name, def); */
/*     } break; */

/*     case Stmt::Kind::ASSIGN: { */
/*       auto assign = (AssignStmt*)stmt.get(); */
/*       auto defVar = sm_.LookupVariable(assign->name->sval); */
/*       if (!defVar) { */
/*         throw CompileError::BuildPosFmt(assign->GetPos(), "undeclared var
 * %s", */
/*                                          *assign->name->sval.c_str()); */
/*       } */
/*       if (!defVar->IsMut()) { */
/*         throw CompileError::BuildPosFmt(assign->GetPos(), */
/*                                          "variable %s is mutable", */
/*                                          assign->name->sval.c_str()); */
/*       } */
/*       Ty ty; */
/*       llvm::Value* value; */
/*       Build(assign->expr.get(), value, ty); */
/*       if (defVar->ty != ty) { */
/*         throw CompileError::BuildPosFmt(assign->expr->GetPos(), */
/*                                          "assigned expr type doesn't match");
 */
/*       } */

/*       builder_.BuildStore(value, defVar->Value()); */
/*     } break; */

/*     case Stmt::Kind::IF: { */
/*       auto ifStmt = (IfStmt*)stmt.get(); */
/*       Ty condTy; */
/*       llvm::Value* condValue; */
/*       Build(ifStmt->cond.get(), condValue, condTy); */
/*       if (condTy != Ty::BOOL) { */
/*         throw CompileError::BuildPosFmt(ifStmt->cond->GetPos(), */
/*                                          "non bool if cond"); */
/*       } */
/*       llvm::Value* cond = */
/*           builder_.BuildICmpNE(condValue,
 * llvm::ConstantInt::getFalse(ctx_)); */

/*       llvm::BasicBlock* thenBB = */
/*           llvm::BasicBlock::Build(ctx_, "then", currentFn_->func); */
/*       llvm::BasicBlock* elseBB = */
/*           llvm::BasicBlock::Build(ctx_, "else", currentFn_->func); */
/*       builder_.BuildCondBr(cond, thenBB, elseBB); */
/*       builder_.SetInsertPoint(thenBB); */

/*       sm_.Push(); */

/*       Build(ifStmt->block.get()); */

/*       sm_.Pop(); */

/*       builder_.BuildBr(elseBB); */
/*       builder_.SetInsertPoint(elseBB); */

/*       if (ifStmt->els) { */
/*         if (ifStmt->els->StmtKind() == Stmt::Kind::IF) { */
/*           auto elsStmt = (IfStmt*)ifStmt->els.get(); */
/*           Build(ifStmt->els); */
/*         } else if (ifStmt->els->StmtKind() == Stmt::Kind::BLOCK) { */
/*           auto elsBlock = (Block*)ifStmt->els.get(); */
/*           sm_.Push(); */
/*           Build(elsBlock); */
/*           sm_.Pop(); */
/*         } */
/*       } */
/*     } break; */
/*     case Stmt::Kind::BLOCK: { */
/*       auto block = (Block*)stmt.get(); */
/*       sm_.Push(); */

/*       Build(block); */

/*       sm_.Pop(); */
/*     } break; */
/*     default: */
/*       std::cerr << "unreachable" << std::endl; */
/*       exit(1); */
/*   } */
/* } */

/* void Builder::Build(Block* block) { */
/*   for (auto& stmt : block->stmts) { */
/*     Build(stmt); */
/*   } */
/* }; */

/* void Builder::Build(std::unique_ptr<FnDecl>& fnDecl, */
/*                     std::shared_ptr<DefFn> def) { */
/*   auto bb = llvm::BasicBlock::Build(ctx_, "", def->func); */
/*   builder_.SetInsertPoint(bb); */

/*   sm_.Push(); */

/*   // args */
/*   auto func = def->func; */
/*   auto it = func->arg_begin(); */
/*   for (int i = 0; i < fnDecl->proto->args.size(); i++) { */
/*     auto name = fnDecl->proto->args.at(i)->name->sval; */
/*     it->setName(name); */

/*     auto defArg = std::make_shared<DefArg>(name, def->args.at(i)); */
/*     defArg->arg = it; */
/*     sm_.InsertVariable(name, defArg); */

/*     it++; */
/*   } */

/*   Build(fnDecl->block.get()); */

/*   sm_.Pop(); */
/* }; */

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

  for (auto& fnDecl : file->fnDecls) {
    auto func = (llvm::Function*)declMap_[fnDecl->decl];
    auto bb = llvm::BasicBlock::Create(ctx_, "", func);
    builder_.SetInsertPoint(bb);

    for (auto& stmt : fnDecl->stmts) {
      BuildStmt(stmt);
    }
  }
};

llvm::Function* Builder::BuildFnProto(std::shared_ptr<Decl> decl) {
  auto ty = (llvm::FunctionType*)GetLLVMTyFromTy(decl->type);
  return llvm::Function::Create(ty, llvm::GlobalValue::ExternalLinkage,
                                decl->name, module_);
}

void Builder::BuildStmt(std::unique_ptr<hir::Stmt>& stmt) {
  switch (stmt->StmtKind()) {
    case hir::Stmt::EXPR:
      BuildExpr((hir::Expr*)stmt.get());
      break;
    case hir::Stmt::RET:
      BuildRetStmt((hir::RetStmt*)stmt.get());
      break;
    case hir::Stmt::VAR_DECL:
      BuildVarDeclStmt((hir::VarDeclStmt*)stmt.get());
      break;
    case hir::Stmt::ASSIGN:
      BuildAssignStmt((hir::AssignStmt*)stmt.get());
      break;
    case hir::Stmt::IF:
      BuildIfStmt((hir::IfStmt*)stmt.get());
      break;
    case hir::Stmt::BLOCK:
      BuildBlock((hir::Block*)stmt.get());
      break;
  }
}

void Builder::BuildRetStmt(hir::RetStmt* stmt) {
  if (stmt->expr) {
    auto value = BuildExpr(stmt->expr.get());
    builder_.CreateRet(value);
  } else {
    builder_.CreateRetVoid();
  }
}

void Builder::BuildVarDeclStmt(hir::VarDeclStmt* stmt) {
  auto value = BuildExpr(stmt->expr.get());
  llvm::AllocaInst* alloca = builder_.CreateAlloca(
      GetLLVMTyFromTy(stmt->decl->type), nullptr, stmt->decl->name);
  builder_.CreateStore(value, alloca);
  declMap_[stmt->decl] = alloca;
}

void Builder::BuildAssignStmt(hir::AssignStmt* stmt) {
  auto value = BuildExpr(stmt->expr.get());
  auto alloca = declMap_[stmt->decl];
  builder_.CreateStore(value, alloca);
}

void Builder::BuildIfStmt(hir::IfStmt* stmt) {}

void Builder::BuildBlock(hir::Block* block) {
  for (auto& stmt : block->stmts) {
    BuildStmt(stmt);
  }
}

llvm::Value* Builder::BuildBinary(hir::Binary* binary) {
  auto lVal = BuildExpr(binary->lhs.get());
  auto rVal = BuildExpr(binary->rhs.get());
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
      op = binary->Ty()->IsFloat() ? llvm::Instruction::BinaryOps::FAdd
                                   : llvm::Instruction::BinaryOps::Add;
      break;
    case ast::BinOp::SUB:
      op = binary->Ty()->IsFloat() ? llvm::Instruction::BinaryOps::FSub
                                   : llvm::Instruction::BinaryOps::Sub;
      break;
    case ast::BinOp::MUL:
      op = binary->Ty()->IsFloat() ? llvm::Instruction::BinaryOps::FMul
                                   : llvm::Instruction::BinaryOps::Mul;
      break;
    case ast::BinOp::DIV:
      op = binary->Ty()->IsFloat() ? llvm::Instruction::BinaryOps::FDiv
                                   : llvm::Instruction::BinaryOps::SDiv;
      break;
    case ast::BinOp::MOD:
      op = binary->Ty()->IsFloat() ? llvm::Instruction::BinaryOps::FRem
                                   : llvm::Instruction::BinaryOps::SRem;
      break;
  }

  return builder_.CreateBinOp(op, lVal, rVal);
}

llvm::Value* Builder::BuildExpr(hir::Expr* expr) {
  std::cout << "BuildExpr " << expr << std::endl;
  switch (expr->ExprKind()) {
    case hir::Expr::Kind::BINARY: {
      std::cout << "Binary " << std::endl;
      auto binary = (hir::Binary*)expr;
      return BuildBinary(binary);
    } break;
    case hir::Expr::Kind::VALUE: {
      auto value = (hir::Value*)expr;
      switch (value->ValueKind()) {
        case hir::Value::Kind::VARIABLE: {
          std::cout << "Variable " << std::endl;
          auto var = (hir::Variable*)value;
          auto alloc = declMap_[var->decl];
          return builder_.CreateLoad(GetLLVMTyFromTy(expr->Ty()), alloc,
                                     var->decl->name);
        } break;
        case hir::Value::Kind::CONSTANT:
          return BuildConstant((hir::Constant*)value);
      }
    } break;
    case hir::Expr::Kind::CALL: {
      std::cout << "Call " << std::endl;
      auto call = (hir::Call*)expr;
      std::vector<llvm::Value*> argValues(call->args.size());
      for (int i = 0; i < call->args.size(); i++) {
        std::cout << "CallArg " << i << std::endl;
        argValues[i] = BuildExpr(call->args[i].get());
      }
      auto fnType = declMap_[call->decl];
      return llvm::CallInst::Create(fnType, argValues, call->decl->name);
    } break;
    case hir::Expr::Kind::UNARY: {
      std::cout << "Unary " << std::endl;
      auto unary = (hir::Unary*)expr;
    } break;
  }
  std::cerr << "unimplemented expr" << std::endl;
  exit(1);
}

llvm::Constant* Builder::BuildConstant(hir::Constant* constant) {
  std::cout << "BuildConstant" << std::endl;
  switch (constant->ConstantKind()) {
    case hir::Constant::Kind::INT: {
      auto v = (hir::IntConstant*)constant;
      return llvm::ConstantInt::getSigned(GetLLVMTyFromTy(v->Ty()), v->val);
    } break;
    case hir::Constant::Kind::FLOAT: {
      auto v = (hir::FloatConstant*)constant;
      return llvm::ConstantFP::get(GetLLVMTyFromTy(v->Ty()), v->val);
    } break;
    case hir::Constant::Kind::CHAR: {
      std::cout << "Char " << std::endl;
      auto v = (hir::CharConstant*)constant;
      return llvm::ConstantInt::getSigned(GetLLVMTyFromTy(v->Ty()), v->val);
    } break;
    case hir::Constant::Kind::BOOL: {
      auto v = (hir::BoolConstant*)constant;
      return v->val ? llvm::ConstantInt::getTrue(ctx_)
                    : llvm::ConstantInt::getFalse(ctx_);
    } break;
    case hir::Constant::Kind::STRING: {
      auto v = (hir::StringConstant*)constant;
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
