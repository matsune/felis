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
    Write(ToString((const std::shared_ptr<mir::Value>&)fn->args.at(i)));
  }
  Down(") -> %s {", ToString(func->type->ret).c_str());

  for (auto& val : fn->vals) {
    Writeln(ToString(val));
  }
  auto bb = fn->entry_bb;
  auto bb_id = 0;
  while (bb) {
    Down("bb%d: {", bb_id++);
    for (auto& inst : bb->instructions) {
      switch (inst->InstKind()) {
        case mir::Inst::Kind::STORE:
          PrintStore((const std::shared_ptr<mir::Store>&)inst);
          break;
        case mir::Inst::Kind::LOAD:
          PrintLoad((const std::shared_ptr<mir::Load>&)inst);
          break;
        case mir::Inst::Kind::BINARY:
          PrintBinary((std::shared_ptr<mir::Binary>&)inst);
          break;
        case mir::Inst::Kind::COMP:
          PrintComp((std::shared_ptr<mir::Comp>&)inst);
          break;
        case mir::Inst::Kind::UNARY:
          PrintUnary((std::shared_ptr<mir::Unary>&)inst);
          break;
        case mir::Inst::Kind::CALL:
          PrintCall((std::shared_ptr<mir::Call>&)inst);
          break;
        case mir::Inst::Kind::RET:
          PrintRet((std::shared_ptr<mir::Ret>&)inst);
          break;
        case mir::Inst::Kind::ARRAY:
          PrintArray((std::shared_ptr<mir::Array>&)inst);
          break;
        case mir::Inst::Kind::COND:
          PrintCond((std::shared_ptr<mir::Cond>&)inst);
          break;
      }
    }
    Up("}");
    bb = bb->next_bb;
  }
  Up("}");
}

void MirPrinter::PrintStore(const std::shared_ptr<mir::Store>& inst) {
  Writeln("%s = store %s", ToString(inst->alloc).c_str(),
          ToString(inst->val).c_str());
}

void MirPrinter::PrintLoad(const std::shared_ptr<mir::Load>& inst) {
  Writeln("%s = load %s", ToString(inst->val).c_str(),
          ToString(inst->alloc).c_str());
}

void MirPrinter::PrintBinary(const std::shared_ptr<mir::Binary>& inst) {
  Writeln("%s = %s(%s, %s)", ToString(inst->val).c_str(),
          ToString(inst->op).c_str(), ToString(inst->lhs).c_str(),
          ToString(inst->rhs).c_str());
}

void MirPrinter::PrintComp(const std::shared_ptr<mir::Comp>& inst) {
  Writeln("%s = %s(%s, %s)", ToString(inst->val).c_str(),
          ToString(inst->op).c_str(), ToString(inst->lhs).c_str(),
          ToString(inst->rhs).c_str());
}

void MirPrinter::PrintUnary(const std::shared_ptr<mir::Unary>& inst) {
  Writeln("%s = %s(%s)", ToString(inst->val).c_str(),
          ToString(inst->op).c_str(), ToString(inst->value).c_str());
}

void MirPrinter::PrintCall(const std::shared_ptr<mir::Call>& inst) {
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

void MirPrinter::PrintRet(const std::shared_ptr<mir::Ret>& inst) {
  if (inst->val) {
    Writeln("ret %s", ToString(inst->val).c_str());
  } else {
    Writeln("ret");
  }
}

void MirPrinter::PrintArray(const std::shared_ptr<mir::Array>& inst) {
  Write("%s = array [", ToString(inst->val).c_str());
  for (auto i = 0; i < inst->values.size(); ++i) {
    if (i > 0) {
      Write(", ");
    }
    Write(ToString(inst->values.at(i)).c_str());
  }
  Writeln("]");
}

void MirPrinter::PrintCond(const std::shared_ptr<mir::Cond>& inst) {
  UNIMPLEMENTED
}

}  // namespace felis

