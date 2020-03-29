#ifndef FELIS_ANALYSIS_DEF_TABLE_H_
#define FELIS_ANALYSIS_DEF_TABLE_H_

#include <iostream>
#include <map>
#include <memory>
#include <vector>

#include "error/handler.h"
#include "syntax/ast.h"

namespace felis {

enum Ty { UNKNOWN, VOID, INT, CHAR, STRING, FLOAT, BOOL };

using DefId = uint32_t;

struct Def {
  DefId id;
  std::string name;
  Node *ptr;
  uint8_t depth;

  Def(uint8_t depth, std::string name, Node *ptr)
      : depth(depth), name(name), ptr(ptr) {}

  virtual bool isFn() = 0;
};

struct DefFn : Def {
  bool isExt;
  std::vector<Ty> args;
  Ty ret;

  DefFn(uint8_t depth, std::string name, Node *ptr, bool isExt,
        std::vector<Ty> args, Ty ret)
      : Def(depth, name, ptr), isExt(isExt), args(args), ret(ret) {}

  bool isFn() override { return true; }
};

struct DefVar : Def {
  bool isLet;
  Ty ty;

  DefVar(uint8_t depth, std::string name, Node *ptr, bool isLet, Ty ty)
      : Def(depth, name, ptr), isLet(isLet), ty(ty) {}

  bool isFn() override { return false; }
};

// Stack

class Stack {
 public:
  void Insert(std::unique_ptr<Def> def) { defs_.push_back(std::move(def)); }
  virtual DefFn *FindFn(uint8_t depth, std::string name) = 0;
  virtual DefVar *FindVar(uint8_t depth, std::string name) = 0;
  const Def *Get(DefId id);

  // debug
  void Print();

 protected:
  std::vector<std::unique_ptr<Def>> defs_;
};

class GlobalStack : public Stack {
 public:
  DefFn *FindFn(uint8_t depth, std::string name) override;
  DefVar *FindVar(uint8_t depth, std::string name) override;

 private:
  const Def *Find(bool isFn, uint8_t depth, std::string name);
};

class LocalStack : public Stack {
 public:
  void Push();
  void Pop();
  DefFn *FindFn(uint8_t depth, std::string name) override;
  DefVar *FindVar(uint8_t depth, std::string name) override;

 private:
  const Def *Find(bool isFn, uint8_t depth, std::string name);
  uint16_t base_ = 0;
};

// DefTable

class DefTable {
 public:
  bool IsDeclaredFn(uint8_t depth, std::string name);
  bool IsDeclaredVar(uint8_t depth, std::string name);
  DefFn *FindFn(uint8_t depth, std::string name);
  DefVar *FindVar(uint8_t depth, std::string name);
  DefId InsertFn(std::unique_ptr<DefFn>);
  DefId InsertVar(std::unique_ptr<DefVar>);
  void PushLocal() { local_.Push(); };
  void PopLocal() { local_.Pop(); };
  const Def *Get(DefId defId);

  // debug
  void PrintGlobal();
  void PrintLocal();

 private:
  GlobalStack global_;
  LocalStack local_;
  DefId nextId_;
};

}  // namespace felis

#endif  // FELIS_ANALYSIS_DEF_TABLE_H_
