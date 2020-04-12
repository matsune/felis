#include "builder.h"

#include <llvm/ADT/StringMap.h>
#include <llvm/IR/Function.h>
#include <llvm/MC/SubtargetFeature.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>

#include "string/string.h"

namespace felis {

Ty TyFromIdent(std::unique_ptr<Ident>& ident) {
  if (!ident) return Ty::VOID;
  std::string str = ident->sval;
  if (str == "int") {
    return Ty::INT;
  } else if (str == "bool") {
    return Ty::BOOL;
  } else if (str == "string") {
    return Ty::STRING;
  } else if (str == "char") {
    return Ty::CHAR;
  } else if (str == "float") {
    return Ty::FLOAT;
  } else {
    return Ty::UNKNOWN;
  }
}

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

/* llvm::Type* Builder::getLLVMTyFromTy(Ty ty) { */
/*   switch (ty) { */
/*     case Ty::BOOL: */
/*       return llvm::Type::getInt1Ty(ctx_); */
/*     case Ty::CHAR: */
/*       return llvm::Type::getInt32Ty(ctx_); */
/*       /1* return llvm::Type::getInt8Ty(ctx_); *1/ */
/*     case Ty::INT: */
/*       return llvm::Type::getInt32Ty(ctx_); */
/*     case Ty::FLOAT: */
/*       return llvm::Type::getFloatTy(ctx_); */
/*     case Ty::VOID: */
/*       return llvm::Type::getVoidTy(ctx_); */
/*     default: */
/*       std::cerr << "unimplemented ty" << std::endl; */
/*       exit(1); */
/*   } */
/* } */

/* // check error and insert def to symtab */
/* std::shared_ptr<DefFn> Builder::InsertDefFn( */
/*     bool isExt, const std::unique_ptr<FnProto>& proto) { */
/*   if (!sm_.CanDeclareFn(proto->name->sval)) { */
/*     handler_.Raise(proto->name->GetPos(), */
/*                    format("redeclared function %s",
 * proto->name->sval.c_str())); */
/*     return nullptr; */
/*   } */

/*   auto def = std::make_shared<DefFn>(proto->name->sval); */

/*   for (auto& arg : *proto->args) { */
/*     Ty ty = TyFromIdent(arg->ty); */
/*     if (ty == Ty::UNKNOWN) { */
/*       handler_.Raise(arg->ty->GetPos(), */
/*                      format("unknown arg type %s", arg->ty->sval.c_str()));
 */
/*       return nullptr; */
/*     } */
/*     def->args.push_back(ty); */
/*   } */
/*   def->ret = TyFromIdent(proto->ret); */
/*   if (def->ret == Ty::UNKNOWN) { */
/*     handler_.Raise(proto->ret->GetPos(), */
/*                    format("unknown ret type %s", proto->ret->sval.c_str()));
 */

/*     return nullptr; */
/*   } */

/*   // set callee */
/*   std::vector<llvm::Type*> args; */
/*   for (auto& arg : def->args) { */
/*     args.push_back(getLLVMTyFromTy(arg)); */
/*   } */
/*   auto funcTy = llvm::FunctionType::get(getLLVMTyFromTy(def->ret), args,
 * false); */
/*   def->func = llvm::Function::Create(funcTy,
 * llvm::GlobalValue::ExternalLinkage, */
/*                                      def->name, module_); */

/*   sm_.InsertFn(proto->name->sval, def); */

/*   return def; */
/* } */

/* bool Builder::Build(Expr* expr, llvm::Value*& value, Ty& ty) { */
/*   switch (expr->ExprKind()) { */
/*     case Expr::Kind::IDENT: { */
/*       auto ident = (Ident*)expr; */
/*       auto def = sm_.LookupVariable(ident->sval); */
/*       if (def == nullptr) { */
/*         handler_.Raise(expr->GetPos(), */
/*                        format("undefined %s", ident->sval.c_str())); */
/*         return false; */
/*       } */
/*       ty = def->ty; */
/*       value = def->Value(); */
/*       if (value->getType()->isPointerTy()) { */
/*         value = builder_.CreateLoad(getLLVMTyFromTy(ty), value); */
/*       } */
/*     } break; */
/*     case Expr::Kind::BINARY: { */
/*       auto binary = (BinaryExpr*)expr; */
/*       Ty lhsTy; */
/*       llvm::Value* lhsValue; */
/*       bool ok = Build(binary->lhs.get(), lhsValue, lhsTy); */
/*       if (!ok) return false; */

/*       Ty rhsTy; */
/*       llvm::Value* rhsValue; */
/*       ok = Build(binary->rhs.get(), rhsValue, rhsTy); */
/*       if (!ok) return false; */

/*       if (lhsTy != rhsTy) { */
/*         // TODO: support binary between another types */
/*         handler_.Raise(binary->GetPos(), "another types binary expr"); */
/*         return false; */
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
/*           handler_.Raise(binary->GetPos(), "unsupported binary expr type");
 */
/*           return false; */
/*       } */
/*       switch (binary->op) { */
/*         case BinOp::ADD: */
/*           value = builder_.CreateAdd(lhsValue, rhsValue); */
/*           break; */
/*         case BinOp::SUB: */
/*           value = builder_.CreateSub(lhsValue, rhsValue); */
/*           break; */
/*         case BinOp::MUL: */
/*           value = builder_.CreateMul(lhsValue, rhsValue); */
/*           break; */
/*         case BinOp::DIV: */
/*           // FIXME: */
/*           value = builder_.CreateFDiv(lhsValue, rhsValue); */
/*           break; */
/*         /1* case BinOp::MOD: *1/ */
/*         /1* value = builder_.Create//(lhsValue, rhsValue); *1/ */
/*         /1* exit(1); *1/ */
/*         /1* break; *1/ */
/*         default: */
/*           std::cout << "UNIMPLEMENTED" << std::endl; */
/*           return false; */
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
/*           return false; */
/*           ty = Ty::STRING; */
/*         } break; */
/*       } */
/*     } break; */
/*     case Expr::Kind::CALL: { */
/*       auto call = (CallExpr*)expr; */
/*       auto def = sm_.LookupFn(call->ident->sval); */
/*       if (def == nullptr) { */
/*         handler_.Raise(call->GetPos(), format("undefined function %s", */
/*                                               call->ident->sval.c_str())); */
/*         return false; */
/*       } */
/*       if (def->args.size() != call->args.size()) { */
/*         handler_.Raise(call->GetPos(), "args count doesn't match"); */
/*         return false; */
/*       } */
/*       std::vector<llvm::Value*> argValues(call->args.size()); */
/*       std::vector<Ty> argTys(call->args.size()); */
/*       for (int i = 0; i < call->args.size(); i++) { */
/*         auto& arg = call->args[i]; */
/*         bool ok = Build(arg.get(), argValues[i], argTys[i]); */
/*         if (!ok) return false; */
/*         if (def->args[i] != argTys[i]) { */
/*           handler_.Raise(arg->GetPos(), "arg type doesn't match"); */
/*           return false; */
/*         } */
/*       } */
/*       ty = def->ret; */
/*       value = llvm::CallInst::Create(def->func, argValues); */
/*     } break; */
/*     case Expr::Kind::UNARY: { */
/*       auto unary = (UnaryExpr*)expr; */
/*       Ty exprTy; */
/*       llvm::Value* exprValue; */
/*       bool ok = Build(unary->expr.get(), exprValue, exprTy); */
/*       if (!ok) return false; */
/*       switch (unary->unOp) { */
/*         case UnOp::NEG: */
/*           if (exprTy == Ty::INT) { */
/*             ty = Ty::INT; */
/*             value = builder_.CreateNeg(exprValue); */
/*           } else if (exprTy == Ty::FLOAT) { */
/*             ty = Ty::FLOAT; */
/*             value = builder_.CreateNeg(exprValue); */
/*           } */
/*           break; */
/*         case UnOp::NOT: */
/*           if (exprTy == Ty::BOOL) { */
/*             ty = Ty::BOOL; */
/*             value = builder_.CreateNot(exprValue); */
/*           } */
/*           break; */
/*       } */
/*     } break; */
/*   } */
/*   return true; */
/* } */

/* bool Builder::Build(std::unique_ptr<Stmt>& stmt) { */
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
/*         bool ok = Build(ret->expr.get(), value, ty); */
/*         if (!ok) return false; */
/*       } else { */
/*         ty = Ty::VOID; */
/*       } */

/*       if (ty == Ty::UNKNOWN) { */
/*         handler_.Raise(ret->expr->GetPos(), format("unknown ret type")); */
/*         return false; */
/*       } */
/*       if (currentFn_->ret != ty) { */
/*         handler_.Raise( */
/*             stmt->GetPos(), */
/*             format("cannot use type %s for function ret type %s", */
/*                    ToString(ty).c_str(), ToString(currentFn_->ret).c_str()));
 */
/*         return false; */
/*       } */
/*       if (ty == Ty::VOID) { */
/*         builder_.CreateRetVoid(); */
/*       } else { */
/*         builder_.CreateRet(value); */
/*       } */
/*     } break; */

/*     case Stmt::Kind::VAR_DECL: { */
/*       auto decl = (VarDeclStmt*)stmt.get(); */
/*       std::string name = decl->name->sval; */
/*       if (!sm_.CanDeclareVariable(name)) { */
/*         handler_.Raise(decl->GetPos(), */
/*                        format("redeclared var %s", name.c_str())); */
/*         return false; */
/*       } */
/*       Ty ty; */
/*       llvm::Value* value; */
/*       bool ok = Build(decl->expr.get(), value, ty); */
/*       if (!ok) return false; */

/*       llvm::AllocaInst* alloca = */
/*           builder_.CreateAlloca(getLLVMTyFromTy(ty), nullptr, name); */
/*       builder_.CreateStore(value, alloca); */

/*       auto def = std::make_shared<DefVar>(name, ty); */
/*       def->value = alloca; */
/*       sm_.InsertVariable(name, def); */
/*     } break; */

/*     case Stmt::Kind::ASSIGN: { */
/*       auto assign = (AssignStmt*)stmt.get(); */
/*       auto defVar = sm_.LookupVariable(assign->name->sval); */
/*       if (!defVar) { */
/*         handler_.Raise(assign->GetPos(), format("undeclared var %s", */
/*                                                 *assign->name->sval.c_str()));
 */
/*         return false; */
/*       } */
/*       if (!defVar->IsMut()) { */
/*         handler_.Raise(assign->GetPos(), format("variable %s is mutable", */
/*                                                 assign->name->sval.c_str()));
 */
/*         return false; */
/*       } */
/*       Ty ty; */
/*       llvm::Value* value; */
/*       bool ok = Build(assign->expr.get(), value, ty); */
/*       if (!ok) return false; */
/*       if (defVar->ty != ty) { */
/*         handler_.Raise(assign->expr->GetPos(), */
/*                        "assigned expr type doesn't match"); */
/*         return false; */
/*       } */

/*       builder_.CreateStore(value, defVar->Value()); */
/*     } break; */

/*     case Stmt::Kind::IF: { */
/*       auto ifStmt = (IfStmt*)stmt.get(); */
/*       Ty condTy; */
/*       llvm::Value* condValue; */
/*       bool ok = Build(ifStmt->cond.get(), condValue, condTy); */
/*       if (!ok) return false; */
/*       if (condTy != Ty::BOOL) { */
/*         handler_.Raise(ifStmt->cond->GetPos(), "non bool if cond"); */
/*         return false; */
/*       } */

/*       llvm::BasicBlock* thenBB = */
/*           llvm::BasicBlock::Create(ctx_, "then", currentFn_->func); */
/*       llvm::BasicBlock* elseBB = */
/*           llvm::BasicBlock::Create(ctx_, "else", currentFn_->func); */
/*       builder_.CreateCondBr(condValue, thenBB, elseBB); */
/*       builder_.SetInsertPoint(thenBB); */

/*       sm_.Push(); */

/*       ok = Build(ifStmt->block.get()); */
/*       if (!ok) return false; */

/*       sm_.Pop(); */

/*       builder_.CreateBr(elseBB); */
/*       builder_.SetInsertPoint(elseBB); */

/*       if (ifStmt->els) { */
/*         if (ifStmt->els->StmtKind() == Stmt::Kind::IF) { */
/*           auto elsStmt = (IfStmt*)ifStmt->els.get(); */
/*           ok = Build(ifStmt->els); */
/*           if (!ok) return false; */
/*         } else if (ifStmt->els->StmtKind() == Stmt::Kind::BLOCK) { */
/*           auto elsBlock = (Block*)ifStmt->els.get(); */
/*           sm_.Push(); */
/*           ok = Build(elsBlock); */
/*           if (!ok) return false; */
/*           sm_.Pop(); */
/*         } */
/*       } */
/*     } break; */
/*     case Stmt::Kind::BLOCK: { */
/*       auto block = (Block*)stmt.get(); */
/*       sm_.Push(); */

/*       bool ok = Build(block); */
/*       if (!ok) return false; */

/*       sm_.Pop(); */
/*     } break; */
/*     default: */
/*       return false; */
/*   } */
/*   return true; */
/* } */

/* bool Builder::Build(Block* block) { */
/*   for (auto& stmt : block->stmts) { */
/*     bool ok = Build(stmt); */
/*     if (!ok) return false; */
/*   } */
/*   return true; */
/* }; */

/* bool Builder::Build(std::unique_ptr<FnDecl>& fnDecl, */
/*                     std::shared_ptr<DefFn> def) { */
/*   auto bb = llvm::BasicBlock::Create(ctx_, "", def->func); */
/*   builder_.SetInsertPoint(bb); */

/*   sm_.Push(); */

/*   // args */
/*   auto func = def->func; */
/*   auto it = func->arg_begin(); */
/*   for (int i = 0; i < fnDecl->proto->args->size(); i++) { */
/*     auto name = fnDecl->proto->args->at(i)->name->sval; */
/*     it->setName(name); */

/*     auto defArg = std::make_shared<DefArg>(name, def->args.at(i)); */
/*     defArg->arg = it; */
/*     sm_.InsertVariable(name, defArg); */

/*     it++; */
/*   } */

/*   bool ok = Build(fnDecl->block.get()); */
/*   if (!ok) return false; */

/*   sm_.Pop(); */

/*   return ok; */
/* }; */

/* bool Builder::Build(std::unique_ptr<File> file) { */
/*   std::map<NodeId, std::shared_ptr<DefFn>> defFnMap; */
/*   for (auto& ext : file->externs) { */
/*     auto def = InsertDefFn(true, ext->proto); */
/*   } */
/*   for (auto& fnDecl : file->fnDecls) { */
/*     auto def = InsertDefFn(false, fnDecl->proto); */
/*     if (def) defFnMap[fnDecl->id] = def; */
/*   } */
/*   if (handler_.HasError()) { */
/*     return false; */
/*   } */

/*   for (auto& fnDecl : file->fnDecls) { */
/*     auto def = defFnMap[fnDecl->id]; */
/*     currentFn_ = def; */
/*     bool ok = Build(fnDecl, def); */
/*     currentFn_ = nullptr; */
/*     if (!ok) return false; */
/*   } */

/*   module_.print(llvm::outs(), nullptr); */

/*   return true; */
/* }; */

/* template <typename T, typename... Args> */
/* ParseResult<T> Parser::Raise(const std::string& fmt, Args... args) { */
/*   return ParseResult<T>::Err(Peek()->pos, format(fmt, args...)); */
/* } */

}  // namespace felis
