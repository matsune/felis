#include "ty_inferer.h"

#include <algorithm>

namespace felis {

void DefTable::InsertFn(std::unique_ptr<FnProto> &fnProto, Scope scope) {
  std::string name = fnProto->name->sval;
  DefKind kind = DefKind::DEF_FN;
  Ty ty = Ty::INT;  // FIXME

  auto it = std::find_if(defs_.begin(), defs_.end(),
                         [name, scope](const std::unique_ptr<Def> &def) {
                           return def->name == name && def->scope == scope;
                         });
  if (it != defs_.end()) {
    auto &def = *it;
    // TODO: Error Handling
    std::cerr << "redeclared function " << def->name << std::endl;
    return;
  }
  defs_.push_back(std::make_unique<Def>(name, kind, ty, scope, fnProto.get()));
}

void TyInferer::Parse(std::unique_ptr<File> &file) {
  for (auto &ext : file->externs) {
    defTable_.InsertFn(ext->proto, scope_);
  }
  for (auto &fn : file->fnDecls) {
    defTable_.InsertFn(fn->proto, scope_);
  }
}

}  // namespace felis
