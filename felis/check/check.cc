#include "check/check.h"

#include <assert.h>

#include "error/error.h"

namespace felis {

void Checker::SetupBuiltin() {
  assert(currentScope_->IsTop());
  // insert basic types into global scope
  currentScope_->InsertType("void",
                            std::make_shared<BasicType>(Type::Kind::VOID));
  currentScope_->InsertType("i32",
                            std::make_shared<BasicType>(Type::Kind::I32));
  currentScope_->InsertType("bool",
                            std::make_shared<BasicType>(Type::Kind::BOOL));
  currentScope_->InsertType("char",
                            std::make_shared<BasicType>(Type::Kind::CHAR));
}

void Checker::Check(std::unique_ptr<File>& file) {
  for (auto& ext : file->externs) {
    InsertFnDecl(true, ext->proto);
  }
  for (auto& fn : file->fnDecls) {
    InsertFnDecl(false, fn->proto);
  }
}

std::shared_ptr<Decl> Checker::InsertFnDecl(
    bool isExt, const std::unique_ptr<FnProto>& proto) {
  if (!CanDecl(proto->name->sval)) {
    throw CompileError::CreatePosFmt(proto->name->GetPos(),
                                     "redeclared function %s",
                                     proto->name->sval.c_str());
  }

  auto fnType = std::make_unique<FuncType>();

  for (auto& arg : proto->args) {
    auto ty = LookupType(arg->ty->sval);
    if (!ty) {
      throw CompileError::CreatePosFmt(arg->ty->GetPos(), "unknown arg type %s",
                                       arg->ty->sval.c_str());
    }
    fnType->args.push_back(ty);
  }
  std::shared_ptr<Type> retTy;
  if (proto->ret) {
    retTy = LookupType(proto->ret->sval);
    if (!retTy) {
      throw CompileError::CreatePosFmt(proto->ret->GetPos(),
                                       "unknown ret type %s",
                                       proto->ret->sval.c_str());
    }
  } else {
    retTy = std::make_shared<BasicType>(Type::Kind::VOID);
  }
  Decl::Kind kind = isExt ? Decl::Kind::EXT : Decl::Kind::FN;
  auto decl =
      std::make_shared<Decl>(proto->name->sval, std::move(fnType), kind);
  currentScope_->InsertDecl(proto->name->sval, decl);
  return decl;
}

void Checker::OpenScope() {
  currentScope_ = std::make_shared<Scope>(currentScope_);
}

void Checker::CloseScope() {
  assert(!currentScope_->IsTop());
  currentScope_ = currentScope_->GetParent();
}

bool Checker::CanDecl(std::string name) {
  return currentScope_->FindDecl(name) == nullptr;
}

std::shared_ptr<Decl> Checker::LookupDecl(std::string name) {
  auto scope = currentScope_;
  while (scope) {
    auto def = scope->FindDecl(name);
    if (def) return def;
    scope = scope->GetParent();
  }
  return nullptr;
}

std::shared_ptr<Type> Checker::LookupType(std::string name) {
  auto scope = currentScope_;
  while (scope) {
    auto ty = scope->FindType(name);
    if (ty) return ty;
    scope = scope->GetParent();
  }
  return nullptr;
}

}  // namespace felis
