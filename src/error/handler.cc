#include "handler.h"

namespace felis {

void ErrorHandler::Raise(Pos pos, std::string message) {
  errors_.push_back(std::make_unique<Error>(pos, message));
}

void ErrorHandler::Raise(std::string message) {
  errors_.push_back(std::make_unique<Error>(message));
}

void ErrorHandler::Report(std::ostream &out) {
  for (auto &error : errors_) {
    out << "felisc error: " << filename_;
    if (error->pos) {
      out << ":" << error->pos->line << ":" << error->pos->column;
    }
    out << ": " << error->message << std::endl;
  }
}

}  // namespace felis
