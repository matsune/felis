#ifndef FELIS_ANALYSIS_TY_INFERER_H_
#define FELIS_ANALYSIS_TY_INFERER_H_

#include <iostream>
#include <map>
#include <memory>
#include <vector>

#include "syntax/ast.h"

namespace felis {

using Scope = uint16_t;

class ScopeResolver {
 public:
  /* Scope Push(Scope current) { */
  /*   Scope nextId = nextId_; */
  /*   nextId_++; */
  /*   parentMap_.insert(nextId, current); */
  /*   return nextId; */
  /* }; */

 private:
  std::map<Scope, Scope> parentMap_;
  Scope nextId_ = 1;
};

enum DefKind { DEF_FN, DEF_VAR };

enum Ty { INT };

struct Def {
  std::string name;
  DefKind kind;
  Ty ty;
  Scope scope;
  Node *ptr;

  Def(std::string name, DefKind kind, Ty ty, Scope scope, Node *ptr)
      : name(name), kind(kind), ty(ty), scope(scope), ptr(ptr) {}
};

class DefTable {
 public:
  void InsertFn(std::unique_ptr<FnProto> &, Scope);

 private:
  std::vector<std::unique_ptr<Def>> defs_;
};

class TyInferer {
 public:
  void Parse(std::unique_ptr<File> &file);

 private:
  Scope scope_ = 0;
  DefTable defTable_;
};

}  // namespace felis

#endif  // FELIS_ANALYSIS_TY_INFERER_H_

