#include "check/check.h"

#include <assert.h>

#include "error/error.h"

namespace felis {

void Checker::SetupBuiltin() {
  assert(currentScope_->IsTop());
  // insert basic types into global scope
  currentScope_->InsertType("void", Type(Type::Kind::VOID));
  currentScope_->InsertType("i32", Type(Type::Kind::I32));
  currentScope_->InsertType("bool", Type(Type::Kind::BOOL));
  currentScope_->InsertType("char", Type(Type::Kind::CHAR));
}

void Checker::Check(std::unique_ptr<File>& file) {
  for (auto& ext : file->externs) {
    InsertFnDecl(true, ext->proto);
  }
  for (auto& fn : file->fnDecls) {
    InsertFnDecl(false, fn->proto);
  }

  for (auto& fn : file->fnDecls) {
    Check(fn);
  }
}

void Checker::Check(std::unique_ptr<FnDecl>& decl) {
  OpenScope();
  for (auto& arg : decl->proto->args) {
    // arg-name duplication is already checked in parser
    currentScope_->InsertDecl(
        arg->name->sval,
        std::make_shared<Decl>(arg->name->sval, LookupType(arg->ty->sval),
                               Decl::Kind::ARG));
  }
  CloseScope();
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
    if (ty.IsUnknown()) {
      throw CompileError::CreatePosFmt(arg->ty->GetPos(), "unknown arg type %s",
                                       arg->ty->sval.c_str());
    }
    fnType->args.push_back(ty);
  }
  Type retTy;
  if (proto->ret) {
    retTy = LookupType(proto->ret->sval);
    if (retTy.IsUnknown()) {
      throw CompileError::CreatePosFmt(proto->ret->GetPos(),
                                       "unknown ret type %s",
                                       proto->ret->sval.c_str());
    }
  } else {
    retTy = Type(Type::Kind::VOID);
  }
  Decl::Kind kind = isExt ? Decl::Kind::EXT : Decl::Kind::FN;
  auto decl = std::make_shared<Decl>(proto->name->sval, *fnType, kind);
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

Type Checker::LookupType(std::string name) {
  auto scope = currentScope_;
  while (scope) {
    auto ty = scope->FindType(name);
    if (!ty.IsUnknown()) return ty;
    scope = scope->GetParent();
  }
  return Type();
}

}  // namespace felis
