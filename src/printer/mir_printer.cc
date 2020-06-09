#include "printer/mir_printer.h"

#include "string/string.h"

namespace felis {

void MirPrinter::Print(const std::unique_ptr<mir::File>& file) {
  WriteLineNum();
  for (auto& func : file->funcs) {
    PrintFunc(func);
  }
  printf("\n");
}

void MirPrinter::PrintFunc(const std::shared_ptr<mir::Func>& func) {
  if (func->IsExt()) {
    Writeln("ext %s %s", func->name.c_str(), ToString(func->type).c_str());
    return;
  }

  Write("fn %s(", func->name.c_str());
  auto fn = std::dynamic_pointer_cast<mir::Function>(func);
  for (auto i = 0; i < fn->args.size(); ++i) {
    if (i > 0) {
      Write(", ");
    }
    Write(ToString(fn->args.at(i)));
  }
  Down(") -> %s {", ToString(fn->type->ret).c_str());

  for (auto it : fn->value_list) {
    Writeln(ToString(it));
  }

  auto bb = fn->entry_bb;
  while (bb) {
    Down("bb%d: {", bb->id);
    for (auto& inst : bb->instructions) {
      switch (inst->InstKind()) {
        case mir::Inst::Kind::ASSIGN:
          PrintAssign((const std::shared_ptr<mir::AssignInst>&)inst);
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
        case mir::Inst::Kind::ARRAY: {
          auto array_inst = (std::shared_ptr<mir::ArrayInst>&)inst;
          Write("%s = [", ToString(array_inst->var).c_str());
          auto i = 0;
          for (auto value : array_inst->values) {
            if (i > 0) Write(", ");
            Write(ToString(value));
            ++i;
          }
          Writeln("]");
        } break;
          //        case mir::Inst::Kind::GEP: {
          //          auto gep_inst = (std::shared_ptr<mir::GepInst>&)inst;
          //          Writeln("%s = gep %s[%d]",
          //          ToString(gep_inst->var).c_str(),
          //                  ToString(gep_inst->arr).c_str(), gep_inst->idx);
          //        } break;
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

void MirPrinter::PrintAssign(const std::shared_ptr<mir::AssignInst>& inst) {
  Writeln("%s = %s", ToString(inst->into).c_str(),
          ToString(inst->value).c_str());
}

void MirPrinter::PrintUnary(const std::shared_ptr<mir::UnaryInst>& inst) {
  Writeln("%s = %s(%s)", ToString(inst->var).c_str(),
          ToString(inst->op).c_str(), ToString(inst->operand).c_str());
}

void MirPrinter::PrintBinary(const std::shared_ptr<mir::BinaryInst>& inst) {
  Writeln("%s = %s(%s, %s)", ToString(inst->var).c_str(),
          ToString(inst->op).c_str(), ToString(inst->lhs).c_str(),
          ToString(inst->rhs).c_str());
}

void MirPrinter::PrintCmp(const std::shared_ptr<mir::CmpInst>& inst) {
  Writeln("%s = %s(%s, %s)", ToString(inst->var).c_str(),
          ToString(inst->op).c_str(), ToString(inst->lhs).c_str(),
          ToString(inst->rhs).c_str());
}

void MirPrinter::PrintCall(const std::shared_ptr<mir::CallInst>& inst) {
  Write("%s = call %s(", ToString(inst->var).c_str(), inst->func->name.c_str());
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
