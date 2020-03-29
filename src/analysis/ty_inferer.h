#ifndef FELIS_ANALYSIS_TY_INFERER_H_
#define FELIS_ANALYSIS_TY_INFERER_H_

#include <iostream>
#include <map>

#include "def_table.h"
#include "error/handler.h"
#include "syntax/ast.h"

namespace felis {

class TyInferer {
 public:
  TyInferer(ErrorHandler &handler) : handler_(handler) {}
  void Parse(std::unique_ptr<File> &file);

 private:
  ErrorHandler &handler_;
  uint8_t depth_ = 0;
  DefTable defTable_;
  std::map<NodeId, DefId> idMap_;
  const DefFn *currentFn_;

  template <typename... Args>
  void Raise(Pos pos, const std::string &fmt, Args... args);

  void InsertFnProto(NodeId nodeId, bool isExt,
                     const std::unique_ptr<FnProto> &);
  void Infer(std::unique_ptr<FnDecl> &);
  void Infer(std::unique_ptr<Stmt> &);
  void Infer(Block *);

  bool InferTy(Ty &, Expr *);

  void PushScope();
  void PopScope();
};

}  // namespace felis

#endif  // FELIS_ANALYSIS_TY_INFERER_H_

