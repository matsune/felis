#include "printer/mir_printer.h"

#include "string/string.h"

namespace felis {

void MirPrinter::Print(const std::unique_ptr<mir::File>& file) {
  WriteLineNum();
  for (auto& func : file->funcs) {
    PrintFunc(func);
  }
}

void MirPrinter::PrintFunc(const std::shared_ptr<mir::Func>& func) {
  if (func->IsExt()) {
    Writeln("ext %d %s %s", func->id, func->name.c_str(),
            ToString(func->type).c_str());
    return;
  }

  Write("%d fn %s(", func->id, func->name.c_str());
  auto fn = std::dynamic_pointer_cast<mir::Function>(func);
  for (auto i = 0; i < fn->args.size(); ++i) {
    if (i > 0) {
      Write(", ");
    }
    Write(ToString(fn->args.at(i)));
  }
  auto ret = fn->ret ? ToString(fn->ret) : "void";
  Down(") -> %s {", ret.c_str());

  auto bb = fn->entry_bb;
  while (bb) {
    Down("bb%d: {", bb->id);
    for (auto& inst : bb->instructions) {
      switch (inst->InstKind()) {
        case mir::Inst::Kind::ALLOC:
          PrintAlloc((const std::shared_ptr<mir::AllocInst>&)inst);
          break;
        case mir::Inst::Kind::LOAD:
          PrintLoad((const std::shared_ptr<mir::LoadInst>&)inst);
          break;
        case mir::Inst::Kind::STORE:
          PrintStore((const std::shared_ptr<mir::StoreInst>&)inst);
          break;
        case mir::Inst::Kind::UNARY:
          PrintUnary((std::shared_ptr<mir::UnaryInst>&)inst);
          break;
        case mir::Inst::Kind::BINARY:
          PrintBinary((std::shared_ptr<mir::BinaryInst>&)inst);
          break;
        case mir::Inst::Kind::CMP:
          PrintCmp((std::shared_ptr<mir::CmpInst>&)inst);
          break;
        case mir::Inst::Kind::ARRAY:
          PrintArray((std::shared_ptr<mir::ArrayInst>&)inst);
          break;
        case mir::Inst::Kind::CALL:
          PrintCall((std::shared_ptr<mir::CallInst>&)inst);
          break;
        case mir::Inst::Kind::BR:
          PrintBr((std::shared_ptr<mir::BrInst>&)inst);
          break;
        case mir::Inst::Kind::GOTO:
          PrintGoto((std::shared_ptr<mir::GotoInst>&)inst);
          break;
        case mir::Inst::Kind::RET:
          PrintRet((std::shared_ptr<mir::RetInst>&)inst);
          break;
      }
    }
    Up("}");
    bb = bb->next_bb;
  }
  Up("}");
}

void MirPrinter::PrintAlloc(const std::shared_ptr<mir::AllocInst>& inst) {
  Writeln("%s = alloc %s", ToString(inst->lval).c_str(),
          ToString(inst->type).c_str());
}

void MirPrinter::PrintLoad(const std::shared_ptr<mir::LoadInst>& inst) {
  Writeln("%s = load %s", ToString(inst->rval).c_str(),
          ToString(inst->lval).c_str());
}

void MirPrinter::PrintStore(const std::shared_ptr<mir::StoreInst>& inst) {
  Writeln("%s = store %s", ToString(inst->lval).c_str(),
          ToString(inst->rval).c_str());
}

void MirPrinter::PrintUnary(const std::shared_ptr<mir::UnaryInst>& inst) {
  Writeln("%s = %s(%s)", ToString(inst->val).c_str(),
          ToString(inst->op).c_str(), ToString(inst->operand).c_str());
}

void MirPrinter::PrintBinary(const std::shared_ptr<mir::BinaryInst>& inst) {
  Writeln("%s = %s(%s, %s)", ToString(inst->val).c_str(),
          ToString(inst->op).c_str(), ToString(inst->lhs).c_str(),
          ToString(inst->rhs).c_str());
}

void MirPrinter::PrintCmp(const std::shared_ptr<mir::CmpInst>& inst) {
  Writeln("%s = %s(%s, %s)", ToString(inst->val).c_str(),
          ToString(inst->op).c_str(), ToString(inst->lhs).c_str(),
          ToString(inst->rhs).c_str());
}

void MirPrinter::PrintArray(const std::shared_ptr<mir::ArrayInst>& inst) {
  Write("%s = [", ToString(inst->val).c_str());
  for (auto i = 0; i < inst->values.size(); ++i) {
    if (i > 0) {
      Write(", ");
    }
    Write(ToString(inst->values.at(i)).c_str());
  }
  Writeln("]");
}

void MirPrinter::PrintCall(const std::shared_ptr<mir::CallInst>& inst) {
  Write("%s = call %d %s(", ToString(inst->val).c_str(), inst->func->id,
        inst->func->name.c_str());
  auto i = 0;
  for (auto& arg : inst->args) {
    if (i > 0) {
      Write(", ");
    }
    Write("%s", ToString(arg).c_str());
    i++;
  }
  Writeln(")");
}

void MirPrinter::PrintBr(const std::shared_ptr<mir::BrInst>& inst) {
  Writeln("br(%s) [then: bb%d, otherwise bb%d]", ToString(inst->cond).c_str(),
          inst->then_bb->id, inst->else_bb->id);
}

void MirPrinter::PrintGoto(const std::shared_ptr<mir::GotoInst>& inst) {
  Writeln("goto -> bb%d", inst->goto_bb->id);
}

void MirPrinter::PrintRet(const std::shared_ptr<mir::RetInst>& inst) {
  if (inst->val) {
    Writeln("ret %s", ToString(inst->val).c_str());
  } else {
    Writeln("ret");
  }
}

}  // namespace felis

