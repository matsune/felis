#ifndef FELIS_ERR_RESULT_H_
#define FELIS_ERR_RESULT_H_

#include <iostream>
#include <memory>
#include <string>
#include <vector>

namespace felis

{

template <class T, class E>
class Result {
 public:
  Result(const Result &) = delete;
  Result(Result &&) = default;

  Result(std::unique_ptr<T> &&t) : t_(std::move(t)), e_(nullptr), ok_(true){};

  Result(std::unique_ptr<E> &&e) : t_(nullptr), e_(std::move(e)), ok_(false){};

  operator bool() const { return ok_; };

  template <class... Args>
  static Result Ok(Args &&... args) {
    return Result(std::make_unique<T>(args...));
  }

  static Result Ok(T *t) { return Result(std::unique_ptr<T>(t)); }
  static Result Ok(std::unique_ptr<T> t) { return Result(std::move(t)); }

  template <class... Args>
  static Result Err(Args &&... args) {
    return Result(std::make_unique<E>(args...));
  }

  static Result Err(E *e) { return Result(std::unique_ptr<E>(e)); }

  template <class TT>
  Result<TT, E> Raise() {
    return Result<TT, E>(move(this->e_));
  }

  bool IsOk() { return ok_; }
  bool IsErr() { return !ok_; }

  std::unique_ptr<T> UnwrapOk() {
    if (IsErr()) {
      std::fprintf(stderr, "Attempting to unwrap an error Result\n");
      std::terminate();
    }
    return std::move(t_);
  }

  std::unique_ptr<E> UnwrapErr() {
    if (IsOk()) {
      std::fprintf(stderr, "Attempting to unwrapErr an ok Result\n");
      std::terminate();
    }
    return std::move(e_);
  }

 private:
  std::unique_ptr<T> t_;
  std::unique_ptr<E> e_;
  bool ok_;
};

}  // namespace felis

#endif  // FELIS_ERR_RESULT_H_
