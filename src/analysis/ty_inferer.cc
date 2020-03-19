#include "ty_inferer.h"

#include "string/string.h"

namespace felis {

void TyInferer::PrintTable() { defTable_.PrintTable(); }

void TyInferer::Parse(std::unique_ptr<File> &file) {
  for (auto &ext : file->externs) {
    defTable_.InsertFn(ext->proto, scope_);
  }
  for (auto &fn : file->fnDecls) {
    defTable_.InsertFn(fn->proto, scope_);
  }
}

template <typename... Args>
void TyInferer::Raise(Pos pos, const std::string &fmt, Args... args) {
  handler_.Raise(pos, format(fmt, args...));
}

}  // namespace felis
