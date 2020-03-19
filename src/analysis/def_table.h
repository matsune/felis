#ifndef FELIS_ANALYSIS_DEF_TABLE_H_
#define FELIS_ANALYSIS_DEF_TABLE_H_

#include <iostream>
#include <map>
#include <memory>
#include <vector>

#include "error/handler.h"
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

enum Ty { UNKNOWN, VOID, INT, CHAR, STRING, FLOAT, BOOL };

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
  DefTable(ErrorHandler &handler) : handler_(handler){};
  void InsertFn(std::unique_ptr<FnProto> &, Scope);

  // debug
  void PrintTable();

 private:
  std::vector<std::unique_ptr<Def>> defs_;
  ErrorHandler &handler_;
};

}  // namespace felis

#endif  // FELIS_ANALYSIS_DEF_TABLE_H_
