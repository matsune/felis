#include "handler.h"

namespace felis {

void ErrorHandler::Raise(Pos pos, std::string message) {
  errors_.push_back(std::make_unique<Error>(pos, message));
}

void ErrorHandler::Report(std::ostream &out) {
  for (auto &error : errors_) {
    out << "felisc error: " << filename_ << ":" << error->pos.line << ":"
        << error->pos.column << ": " << error->message << std::endl;
  }
}

}  // namespace felis
