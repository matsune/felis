#include "def_table.h"

#include <algorithm>

#include "string/string.h"

namespace felis {

void DefTable::InsertFn(std::unique_ptr<FnProto> &fnProto, Scope scope) {
  std::string name = fnProto->name->sval;
  DefKind kind = DefKind::DEF_FN;
  Ty ty = Ty::UNKNOWN;
  if (fnProto->ret) {
    std::string tyStr = fnProto->ret->sval;
    if (tyStr == "int") {
      ty = Ty::INT;
    } else if (tyStr == "bool") {
      ty = Ty::BOOL;
    } else if (tyStr == "string") {
      ty = Ty::STRING;
    } else if (tyStr == "char") {
      ty = Ty::CHAR;
    } else if (tyStr == "float") {
      ty = Ty::FLOAT;
    } else {
      ty = Ty::UNKNOWN;
    }
  } else {
    ty = Ty::VOID;
  }

  if (ty == Ty::UNKNOWN) {
    handler_.Raise(fnProto->ret->GetPos(),
                   format("unknown ret type %s", fnProto->ret->sval.c_str()));
    return;
  }

  auto it = std::find_if(defs_.begin(), defs_.end(),
                         [name, scope](const std::unique_ptr<Def> &def) {
                           return def->name == name && def->scope == scope;
                         });
  if (it != defs_.end()) {
    auto &def = *it;
    handler_.Raise(fnProto->name->GetPos(),
                   format("redeclared function %s", def->name.c_str()));
    return;
  }
  defs_.push_back(std::make_unique<Def>(name, kind, ty, scope, fnProto.get()));
}

void DefTable::PrintTable() {
  std::cout << "--------------" << std::endl << "DefTable" << std::endl;
  for (auto const &def : defs_) {
    std::cout << "--------------" << std::endl;
    std::cout << "NAME: " << def->name << std::endl;
    std::cout << "KIND: " << (def->kind == DefKind::DEF_FN ? "fn" : "var")
              << std::endl;
    std::string ty;
    switch (def->ty) {
      case Ty::UNKNOWN:
        ty = "unknown";
      case Ty::VOID:
        ty = "void";
        break;
      case Ty::BOOL:
        ty = "bool";
        break;
      case Ty::INT:
        ty = "int";
        break;
      case Ty::CHAR:
        ty = "char";
        break;
      case Ty::FLOAT:
        ty = "float";
        break;
      case Ty::STRING:
        ty = "string";
        break;
    }
    std::cout << "TY: " << ty << std::endl;
    std::cout << "SCOPE: " << def->scope << std::endl;
  }
}

}  // namespace felis
