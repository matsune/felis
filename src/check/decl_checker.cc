#include "check/decl_checker.h"

#include "check/parse.h"
#include "error/error.h"
#include "macro.h"
#include "unique.h"

namespace felis {

void DeclChecker::InsertBuiltinTypes() {
  assert(current_scope_->IsTop());
  // insert basic types into global scope
  /* current_scope_->InsertType("void", Type::MakeVoid()); */
  current_scope_->InsertType("i8", Type::MakeI8());
  current_scope_->InsertType("i16", Type::MakeI16());
  current_scope_->InsertType("i32", Type::MakeI32());
  current_scope_->InsertType("i64", Type::MakeI64());
  current_scope_->InsertType("f32", Type::MakeF32());
  current_scope_->InsertType("f64", Type::MakeF64());
  current_scope_->InsertType("bool", Type::MakeBool());
  current_scope_->InsertType("string", Type::MakeString());
  // arch size
  current_scope_->InsertType("int", Type::MakeArchInt(is_32bit));
}

std::shared_ptr<Decl> DeclChecker::LookupDecl(const std::string& name) {
  auto scope = current_scope_;
  while (scope) {
    auto def = scope->FindDecl(name);
    if (def) return def;
    scope = scope->GetParent();
  }
  return nullptr;
}

std::shared_ptr<Decl> DeclChecker::LookupVarDecl(const std::string& name) {
  auto decl = LookupDecl(name);
  if (!decl) return nullptr;
  return decl->IsFunc() ? nullptr : decl;
}

std::shared_ptr<Decl> DeclChecker::LookupFuncDecl(const std::string& name) {
  auto decl = LookupDecl(name);
  if (!decl) return nullptr;
  return decl->IsFunc() ? decl : nullptr;
}

std::shared_ptr<Type> DeclChecker::LookupType(const ast::AstNode* node) {
  if (node_isa<ast::Ident>(node)) {
    auto ident = node_cast<ast::Ident>(node);
    auto scope = current_scope_;
    while (scope) {
      auto ty = scope->FindType(ident->val);
      if (ty) return ty;
      scope = scope->GetParent();
    }
  } else {
    auto array = node_cast<ast::ArrayType>(node);
    auto elem = LookupType(array->elem);
    int64_t size;
    std::string err;
    if (!ParseInt(array->size_lit->val, size, err)) {
      throw LocError(array->begin, err);
    }
    return Type::MakeArray(elem, size);
  }
}

void DeclChecker::CheckGlobalLevel(const std::unique_ptr<ast::File>& file) {
  assert(current_scope_->IsTop());

  for (auto& ext : file->externs) {
    auto decl = MakeFnDecl(true, ext->proto);
    current_scope_->InsertDecl(ext->proto->name->val, decl);
  }
  for (auto& fn : file->funcs) {
    auto decl = MakeFnDecl(false, fn->proto);
    current_scope_->InsertDecl(fn->proto->name->val, decl);
  }
}

void DeclChecker::OpenScope() {
  current_scope_ = std::make_shared<Scope>(current_scope_);
}

void DeclChecker::CloseScope() {
  assert(!current_scope_->IsTop());
  current_scope_ = current_scope_->GetParent();
}

bool DeclChecker::ExistsInThisScope(const std::string& name) {
  return current_scope_->FindDecl(name) != nullptr;
}

std::shared_ptr<Decl> DeclChecker::MakeFnDecl(bool is_ext,
                                              const ast::FnProto* proto) {
  if (ExistsInThisScope(proto->name->val)) {
    throw LocError::Create(proto->name->begin, "redeclared function %s",
                           proto->name->val.c_str());
  }

  std::vector<std::shared_ptr<Type>> args;
  for (auto& arg : proto->args->list) {
    auto ty = LookupType(arg->type);
    if (!ty) {
      throw LocError::Create(arg->type->begin, "unknown arg type");
    }
    args.push_back(ty);
  }
  std::shared_ptr<Type> ret_ty;
  if (proto->ret) {
    ret_ty = LookupType(proto->ret);
    if (!ret_ty) {
      throw LocError::Create(proto->ret->begin, "unknown ret type");
    }
  } else {
    ret_ty = Type::MakeVoid();
  }
  auto fn_type = Type::MakeFunc(args, ret_ty);
  DeclKind kind = is_ext ? DeclKind::EXT : DeclKind::FN;
  return std::make_shared<Decl>(proto->name->val, fn_type, kind);
}

}  // namespace felis
