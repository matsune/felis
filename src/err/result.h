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

  ~Result() {
    if (t_) delete t_;
    if (e_) delete e_;
  }

  operator bool() const { return ok_; };

  template <class... Args>
  static Result Ok(Args &&... args) {
    return Result(new T(args...));
  }

  static Result Ok(T *t) { return Result(t); }

  template <class... Args>
  static Result Err(Args &&... args) {
    return Result(new E(args...));
  }

  static Result Err(E *e) { return Result(e); }

  template <class TT>
  Result<TT, E> Raise() {
    return Result<TT, E>::Err(this->UnwrapErr());
  }

  template <class TT>
  Result<TT, E> Into() {
    return Result<TT, E>::Ok((TT *)this->Unwrap());
  }

  bool IsOk() { return ok_; }
  bool IsErr() { return !ok_; }

  // `this` won't manage <T> or <E> pointer after unwrapping,
  // so you need to free pointer at where you call this method.
  T *Unwrap() {
    if (IsErr()) {
      std::fprintf(stderr, "Attempting to unwrap an error Result\n");
      std::terminate();
    }

    T *tmp = t_;
    t_ = nullptr;
    return tmp;
  }

  E *UnwrapErr() {
    if (IsOk()) {
      std::fprintf(stderr, "Attempting to unwrapErr an ok Result\n");
      std::terminate();
    }

    E *tmp = e_;
    e_ = nullptr;
    return tmp;
  }

 private:
  Result(T *t) : t_(t), e_(nullptr), ok_(true){};
  Result(E *e) : t_(nullptr), e_(e), ok_(false){};

  T *t_;
  E *e_;
  bool ok_;
};

}  // namespace felis

#endif  // FELIS_ERR_RESULT_H_
