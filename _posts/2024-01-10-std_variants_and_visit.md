---
layout: post
title:  "简明实现 std::variants & std::visit"
date:   2023-01-10
last_modified_at: 2023-01-11
tag: [CppDevelop]
mathjax: true
---
`cpp17` 标准中引入了 `std::variants` ，提供了类型安全的union，可以用来替代 C 编程中常用的 `tagged union` 技术。类内部会存一个枚举值代表内部的实际类型，使用时根据枚举值进行不同的逻辑处理，差不多这样：
```c
struct Fruit {
  enum FruitType { 
    type_apple, 
    type_banana, 
    type_cherry
  } type;

  union {
    Apple apple;
    Banana banana;
    Peach type_cherry;
  }
};

//...when using it
switch (fruit.type) {
  case type_apple:
    // ... 
  case type_banana:
    // ...
  case type_cherry:
  // ...
}
```
可以方便的定义该类型为`std::variant<Apple,Banana,Cherry>`并配套使用`std::visit`根据类型进行模式匹配。

```cpp
#include <algorithm>
#include <functional>
#include <limits>
#include <tuple>
#include <type_traits>

namespace dev {
template <class... Tys> class variant {
private:
  template <size_t P, class T, class... Ts>
  struct PickInTypeList : PickInTypeList<P - 1, Ts...> {};

  template <class T, class... Ts>
  struct PickInTypeList<0, T, Ts...> { 
    using type = T;
  };

  static constexpr size_t get_index_type_id_() {
    if constexpr (sizeof...(Tys) < std::numeric_limits<short>::max())
      return 0;
    else if constexpr (sizeof...(Tys) < std::numeric_limits<int>::max())
      return 1;
    else if constexpr (sizeof...(Tys) < std::numeric_limits<long long>::max())
      return 2;
  }

  using IndexType =
      typename PickInTypeList<get_index_type_id_(), unsigned short,
                              unsigned int, unsigned long long>::type;

  template <IndexType I, class Tx, class Ty, class... Tzs>
  struct FindIndex : FindIndex<I + 1, Tx, Tzs...> {};

  template <IndexType I, class T, class... Ts>
  struct FindIndex<I, T, T, Ts...> : std::integral_constant<std::size_t, I> {};

private: // internal typed instance
  template <class Ty> class variantImpl {
  public:
    static constexpr IndexType index() { return s_index_; }
    Ty &value() { return value_; }
    variantImpl(Ty &&val) : value_(std::forward<Ty>(val)) {}

  private:
    Ty value_;
    static constexpr IndexType s_index_ = FindIndex<0, Ty, Tys...>::value;
  };

public:
  static constexpr auto kBufferSize = std::max({sizeof(variantImpl<Tys>)...});
  static constexpr auto kAlignment = std::max({alignof(variantImpl<Tys>)...});

  IndexType index() const noexcept { return index_; }

  template <class Ty> variant(Ty &&val) noexcept {
    using Tyd = std::decay_t<Ty &&>;
    ::new (pimpl<Tyd>()) variantImpl<Ty>(std::forward<Ty>(val));
    index_ = pimpl<Tyd>()->index();
  }

  template <class Ty> variant &operator=(Ty &&val) noexcept {
    variant temp(std::forward<Ty>(val));
    std::swap(temp.index_, index_);
    std::swap(temp.buffer_, buffer_);
    return *this;
  }

  variant() = default;
  variant(variant &&) = default;
  variant(variant const &) = default;
  ~variant() = default;

  template <class Ty> Ty &cast() {
    using Tyd = std::decay_t<Ty>;
    if (FindIndex<0, Tyd, Tys...>::value == index())
      return pimpl<Tyd>()->value();

    throw std::bad_cast{};
  }

  template <size_t I> auto &get() {
    if (index() == I)
      return pimpl<typename PickInTypeList<I, Tys...>::type>()->value();

    throw std::bad_cast{};
  }

  static constexpr size_t variants_size() { return sizeof...(Tys); }

private:
  template <class Ty> variantImpl<Ty> *pimpl() {
    return reinterpret_cast<variantImpl<Ty> *>(buffer_);
  }

  template <class Ty> const variantImpl<Ty> *pimpl() const {
    return reinterpret_cast<const variantImpl<Ty> *>(buffer_);
  }

private:
  alignas(kAlignment) std::byte buffer_[kBufferSize];
  IndexType index_;
};

template <bool IsValid, typename R> struct dispatcher;

template <typename R> struct dispatcher<false, R> {
  template <std::size_t B, typename F, typename... Vs, size_t... Is>
  static constexpr R switch_(F &&f, Vs &&...vs, std::index_sequence<Is...>) {
    __builtin_unreachable();
  }
};

template <typename R> struct dispatcher<true, R> {
  template <typename F, typename... Vs, std::size_t... Is>
  static R case_(F &&f, Vs &&...vs, std::index_sequence<Is...>) {
    using Expected = R;
    using Actual = decltype(std::invoke(
        std::forward<F>(f), std::forward<Vs>(vs).template get<Is>()...));
    static_assert(
        std::is_same_v<Expected, Actual>,
        "`dev::visit` requires the visitor to have a single return type");

    return std::invoke(std::forward<F>(f),
                       std::forward<Vs>(vs).template get<Is>()...);
  }

  template <std::size_t B, typename F, typename... Vs, size_t... Is>
  static constexpr R switch_(F &&f, Vs &&...vs, std::index_sequence<Is...>) {
    if constexpr (sizeof...(Vs) == sizeof...(Is)) {
      return dispatcher<true, R>::template case_<F, Vs...>(
          std::forward<F>(f), std::forward<Vs>(vs)...,
          std::index_sequence<Is...>{});
    } else {
      static_assert(sizeof...(Vs) > sizeof...(Is),
                    "indices should smaller than variants");
      auto &&v = std::get<sizeof...(Is)>(
          std::forward_as_tuple(std::forward<Vs>(vs)...));
      constexpr std::size_t size = std::decay_t<decltype(v)>::variants_size();
      switch (v.index()) {
      case B + 0:
        return dispatcher<B + 0 < size, R>::template switch_<0, F, Vs...>(
            std::forward<F>(f), std::forward<Vs>(vs)...,
            std::index_sequence<Is..., B + 0>{});
      case B + 1:
        return dispatcher<B + 1 < size, R>::template switch_<0, F, Vs...>(
            std::forward<F>(f), std::forward<Vs>(vs)...,
            std::index_sequence<Is..., B + 1>{});
      case B + 2:
        return dispatcher<B + 2 < size, R>::template switch_<0, F, Vs...>(
            std::forward<F>(f), std::forward<Vs>(vs)...,
            std::index_sequence<Is..., B + 2>{});
      case B + 3:
        return dispatcher<B + 3 < size, R>::template switch_<0, F, Vs...>(
            std::forward<F>(f), std::forward<Vs>(vs)...,
            std::index_sequence<Is..., B + 3>{});
      default:
        return dispatcher<B + 4 < size, R>::template switch_<B + 4, F, Vs...>(
            std::forward<F>(f), std::forward<Vs>(vs)...,
            std::index_sequence<Is...>{});
      }
    }
  }
};

template <class F, class... Vs> auto visit(F &&f, Vs &&...vs) {
  using R = decltype(std::invoke(std::forward<F>(f),
                                 std::forward<Vs>(vs).template get<0>()...));
  return dispatcher<true, R>::template switch_<0, F, Vs...>(
      std::forward<F>(f), std::forward<Vs>(vs)..., std::index_sequence<>{});
}

} // namespace dev

```