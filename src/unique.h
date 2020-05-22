#ifndef FELIS_UNIQUE_H_
#define FELIS_UNIQUE_H_

#include <deque>
#include <memory>

#include "node/node.h"

namespace felis {

template <typename T>
std::unique_ptr<T> unique_cast(std::unique_ptr<Node> ptr) {
  return std::unique_ptr<T>(static_cast<T *>(ptr.release()));
}

template <class T>
class unique_deque {
 public:
  unique_deque<T>() = default;
  unique_deque<T>(const unique_deque<T> &) = delete;
  unique_deque<T>(unique_deque<T> &&) = default;

  inline const std::unique_ptr<T> &operator[](int i) const { return que[i]; }
  inline std::unique_ptr<T> &operator[](int i) { return que[i]; }
  inline const std::unique_ptr<T> &at(int i) const { return que.at(i); }

  inline auto begin() noexcept { return que.begin(); }
  inline auto begin() const noexcept { return que.begin(); }
  inline auto cbegin() const noexcept { return que.cbegin(); }
  inline auto end() noexcept { return que.end(); }
  inline auto end() const noexcept { return que.end(); }
  inline auto cend() const noexcept { return que.cend(); }
  inline auto size() const noexcept { return que.size(); }
  inline bool empty() const noexcept { return que.empty(); }

  inline const std::unique_ptr<T> &front() const { return que.front(); }
  inline const std::unique_ptr<T> &back() const { return que.back(); }

  inline void push_back(std::unique_ptr<T> &&e) { que.push_back(std::move(e)); }

  std::unique_ptr<T> move_front() {
    auto e = std::move(que.front());
    que.pop_front();
    return e;
  }
  std::unique_ptr<T> move_back() {
    auto e = std::move(que.back());
    que.pop_back();
    return e;
  }

 private:
  std::deque<std::unique_ptr<T>> que;
};

}  // namespace felis

#endif  // FELIS_UNIQUE_H_
