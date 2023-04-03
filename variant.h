#pragma once

#include "variant_traits.h"
#include <array>
#include <concepts>
#include <functional>
#include <numeric>
#include <type_traits>

struct bad_variant_access : std::exception {
  const char* what() const noexcept override {
    return "bad variant access";
  }
};

template <typename T>
struct in_place_type_t {};

template <typename T>
inline constexpr in_place_type_t<T> in_place_type = in_place_type_t<T>{};

template <size_t ind>
struct in_place_index_t {
  explicit in_place_index_t() = default;
};

template <typename... Tp>
class variant;

template <size_t ind>
inline constexpr in_place_index_t<ind> in_place_index{};

template <typename Visitor, typename... Variants>
constexpr decltype(auto) visit(Visitor&& vis, Variants&&... vars);

namespace variant_ns {
template <bool E, typename... Tp>
struct unions_holder {}; // monostate

template <typename Visitor, typename Variant, typename... Variants>
constexpr void visit_with_ind(Visitor&& vis, size_t ind, Variant&& var, Variants&&... vars);

template <bool trivially_destructible, typename T, typename... Tp>
struct unions_holder<trivially_destructible, T, Tp...> {
  constexpr unions_holder() : next_() {}
  constexpr ~unions_holder() = default;

  union {
    T data_;
    unions_holder<trivially_destructible, Tp...> next_;
  };
};

template <typename T, typename... Tp>
struct unions_holder<false, T, Tp...> {
  constexpr unions_holder() : next_() {}
  constexpr ~unions_holder() {}

  union {
    T data_;
    unions_holder<false, Tp...> next_;
  };
};

template <bool E, typename... Tp>
void reset(unions_holder<E, Tp...>& var, size_t ind) {}

template <typename T, typename... Tp>
void reset(unions_holder<false, T, Tp...>& var, size_t ind) {
  if (ind == 0) {
    var.data_.~T();
  } else {
    reset(var.next_, ind - 1);
  }
}

template <bool enable, typename... Tp>
struct destructor_base {
  constexpr destructor_base() = default; // holds monostate
  constexpr ~destructor_base() = default;

  unions_holder<enable, Tp...> data_;
  size_t index_{variant_npos};
};

template <typename... Tp>
struct destructor_base<false, Tp...> {
  constexpr destructor_base() = default; // holds monostate
  constexpr ~destructor_base() {
    if (index_ != variant_npos) {
      reset(data_, index_);
    }
  }

  unions_holder<false, Tp...> data_;
  size_t index_{variant_npos};
};

template <typename S>
struct is_union_storage : std::false_type {};

template <bool E, typename... Tp>
struct is_union_storage<unions_holder<E, Tp...>> : std::true_type {};

template <typename S>
inline constexpr bool is_union_storage_t = is_union_storage<std::remove_cvref_t<S>>::value;

template <size_t ind, typename Storage>
requires(is_union_storage_t<Storage>) static constexpr auto&& get_impl(Storage&& storage) {
  if constexpr (ind == 0) {
    return storage.data_;
  } else {
    return get_impl<ind - 1>(storage.next_);
  }
}
} // namespace variant_ns

template <typename Variant>
struct variant_size;

template <typename Variant>
struct variant_size<const Variant> : variant_size<Variant> {};

template <typename Variant>
struct variant_size<volatile Variant> : variant_size<Variant> {};

template <typename Variant>
struct variant_size<const volatile Variant> : variant_size<Variant> {};

template <typename... Tp>
struct variant_size<variant<Tp...>> : std::integral_constant<size_t, sizeof...(Tp)> {};

template <typename T>
inline constexpr size_t variant_size_v = variant_size<T>::value;

template <size_t ind, typename Variant>
struct variant_alternative;

template <size_t ind, typename... Tp>
struct variant_alternative<ind, variant<Tp...>> : variant_ns::ind_to_type<ind, Tp...> {};

template <size_t ind, typename Variant>
using variant_alternative_t = typename variant_alternative<ind, Variant>::type;

template <size_t ind, typename Variant>
struct variant_alternative<ind, const Variant> {
  using type = std::add_const_t<variant_alternative_t<ind, Variant>>;
};

template <size_t ind, typename Variant>
struct variant_alternative<ind, volatile Variant> {
  using type = std::add_volatile_t<variant_alternative_t<ind, Variant>>;
};

template <size_t ind, typename Variant>
struct variant_alternative<ind, const volatile Variant> {
  using type = std::add_cv_t<variant_alternative_t<ind, Variant>>;
};

template <typename... Tp>
class variant : variant_ns::destructor_base<variant_ns::all_of_v<std::is_trivially_destructible, Tp...>, Tp...> {
  constexpr void variant_copy_constructor(variant const& rhs) {
    this->index_ = rhs.index_;
    try {
      visit(
          []<typename A, typename B>(A& a, B const& b) {
            if constexpr (std::is_constructible_v<A, B>) {
              std::construct_at(std::addressof(a), b);
            }
          },
          *this, rhs);
    } catch (...) {
      this->index_ = variant_npos;
      throw;
    }
  }

  constexpr void variant_move_constructor(variant&& rhs) {
    this->index_ = rhs.index_;
    try {
      visit(
          []<typename A, typename B>(A& a, B& b) {
            if constexpr (std::is_constructible_v<A, B>) {
              std::construct_at(std::addressof(a), std::move(b));
            }
          },
          *this, rhs);
    } catch (...) {
      this->index_ = variant_npos;
      throw;
    }
  }

  constexpr void reset() {
    if (valueless_by_exception()) {
      return;
    }
    variant_ns::reset(this->data_, this->index_);
    this->index_ = variant_npos;
  }

  template <size_t ind>
  using alternative_t = variant_ns::ind_to_type_t<ind, Tp...>;
  template <typename T>
  static constexpr size_t alternative_v = variant_ns::type_to_ind_v<T, Tp...>;

  inline static constexpr bool default_constructible_v =
      std::is_default_constructible_v<variant_ns::get_first_t<Tp...>>;
  inline static constexpr bool nothrow_default_constructible_v =
      std::is_nothrow_constructible_v<variant_ns::get_first_t<Tp...>>;

  inline static constexpr bool nothrow_copy_constructible_v =
      variant_ns::all_of_v<std::is_nothrow_copy_constructible, Tp...>;
  inline static constexpr bool trivially_copy_constructible_v =
      variant_ns::all_of_v<std::is_trivially_copy_constructible, Tp...>;
  inline static constexpr bool copy_constructible_v = variant_ns::all_of_v<std::is_copy_constructible, Tp...>;

  inline static constexpr bool nothrow_move_constructible_v =
      variant_ns::all_of_v<std::is_nothrow_move_constructible, Tp...>;
  inline static constexpr bool trivially_move_constructible_v =
      variant_ns::all_of_v<std::is_trivially_move_constructible, Tp...>;
  inline static constexpr bool move_constructible_v = variant_ns::all_of_v<std::is_move_constructible, Tp...>;

  inline static constexpr bool trivially_copy_assignable_v =
      variant_ns::all_of_v<std::is_trivially_copy_assignable, Tp...>;
  inline static constexpr bool copy_assignable_v = variant_ns::all_of_v<std::is_copy_assignable, Tp...>;

  inline static constexpr bool nothrow_move_assiganble_v = variant_ns::all_of_v<std::is_nothrow_move_assignable, Tp...>;
  inline static constexpr bool trivially_move_assignable_v =
      variant_ns::all_of_v<std::is_trivially_move_assignable, Tp...>;
  inline static constexpr bool move_assignable_v = variant_ns::all_of_v<std::is_move_assignable, Tp...>;

  template <typename T>
  inline static constexpr bool exactly_once_v = variant_ns::exactly_once_v<T, Tp...>;

public:
  constexpr variant() noexcept(nothrow_default_constructible_v) requires default_constructible_v
      : variant(in_place_index<0>) {}
  constexpr variant() = delete;
  constexpr variant(variant const& rhs) noexcept(nothrow_copy_constructible_v) requires trivially_copy_constructible_v =
      default;
  constexpr variant(variant const& rhs) noexcept(nothrow_copy_constructible_v) requires copy_constructible_v &&
      (!trivially_copy_constructible_v) {
    variant_copy_constructor(rhs);
  }
  constexpr variant(variant const& rhs) = delete;

  constexpr variant(variant&& rhs) noexcept(nothrow_move_constructible_v) requires trivially_move_constructible_v =
      default;
  constexpr variant(variant&& rhs) noexcept(nothrow_move_constructible_v) requires move_constructible_v &&
      (!trivially_move_constructible_v) {
    variant_move_constructor(std::move(rhs));
  }
  constexpr variant(variant&& rhs) = delete;

  // noexcept not need cppreference
  constexpr variant&
  operator=(variant const& rhs) requires trivially_copy_assignable_v&& trivially_copy_constructible_v = default;
  constexpr variant& operator=(variant const& rhs) requires copy_constructible_v&& copy_assignable_v &&
      (!(trivially_copy_assignable_v && trivially_copy_constructible_v)) {
    if (this == &rhs) {
      return *this;
    }
    if (rhs.valueless_by_exception()) {
      reset();
    } else if (index() == rhs.index()) {
      visit(
          []<typename A, typename B>(A& a, B const& b) {
            if constexpr (std::is_same_v<A, B>) {
              a = b;
            }
          },
          *this, rhs);
    } else {
      reset();
      this->index_ = rhs.index();
      bool need;
      try {
        need = visit(
            []<typename A, typename B>(A& a, B const& b) {
              if constexpr (std::is_same_v<A, B> &&
                            (std::is_nothrow_copy_constructible_v<A> || !std::is_nothrow_move_constructible_v<A>)) {
                std::construct_at(std::addressof(a), b);
                return false;
              } else {
                return true;
              }
            },
            *this, rhs);
      } catch (...) {
        this->index_ = variant_npos;
        throw;
      }
      if (need) {
        variant_move_constructor(variant(rhs));
      }
    }
    return *this;
  }
  constexpr variant& operator=(variant const&) = delete;

  constexpr variant& operator=(variant&& rhs) noexcept(nothrow_move_assiganble_v&& nothrow_move_constructible_v)
      requires trivially_move_assignable_v&& trivially_move_constructible_v = default;
  constexpr variant& operator=(variant&& rhs) noexcept(
      nothrow_move_assiganble_v&& nothrow_move_constructible_v) requires move_constructible_v&& move_assignable_v
      && (!(trivially_move_assignable_v && trivially_move_constructible_v)) {
    if (this == &rhs) {
      return *this;
    }
    if (rhs.valueless_by_exception() && valueless_by_exception()) {
      // do nothing
    } else if (rhs.valueless_by_exception()) {
      reset();
    } else if (index() == rhs.index()) {
      visit(
          []<typename A, typename B>(A& a, B& b) {
            if constexpr (std::is_same_v<A, B>) {
              a = std::move(b);
            }
          },
          *this, rhs);
    } else {
      reset();
      variant_move_constructor(std::move(rhs));
    }
    return *this;
  }
  constexpr variant& operator=(variant&&) = delete;

  constexpr ~variant() = default;

  constexpr void swap(variant& rhs) {
    if (valueless_by_exception() && rhs.valueless_by_exception()) {
      // do nothing
    } else if (valueless_by_exception()) {
      *this = std::move(rhs);
      rhs.reset();
    } else if (rhs.valueless_by_exception()) {
      rhs = std::move(*this);
      reset();
    } else if (index() == rhs.index()) {
      visit(
          []<typename A, typename B>(A& a, B& b) {
            if constexpr (std::is_same_v<A, B>) {
              using std::swap;
              swap(a, b);
            }
          },
          *this, rhs);
    } else {
      std::swap(*this, rhs);
    }
  }

  template <size_t ind, typename Variant>
  friend constexpr auto&& get(Variant&& var);

  template <typename Type>
  requires exactly_once_v<Type> friend constexpr Type& get(variant<Tp...>& var) {
    return get<alternative_v<Type>>(var);
  }
  template <typename Type>
  requires exactly_once_v<Type> friend constexpr Type const& get(variant<Tp...> const& var) {
    return get<alternative_v<Type>>(var);
  }
  template <typename Type>
  requires exactly_once_v<Type> friend constexpr Type&& get(variant<Tp...>&& var) {
    return std::move(get<alternative_v<Type>>(var));
  }
  template <typename Type>
  requires exactly_once_v<Type> friend constexpr Type const&& get(variant<Tp...> const&& var) {
    return std::move(get<alternative_v<Type>>(var));
  }

  template <size_t ind, typename... Args>
      requires(ind < sizeof...(Tp)) &&
      std::is_constructible_v<alternative_t<ind>, Args&&...> constexpr variant(
          in_place_index_t<ind>, Args&&... args) noexcept(noexcept(emplace<ind>(std::forward<Args>(args)...))) {
    emplace<ind>(std::forward<Args>(args)...);
  }

  template <size_t ind, typename... Args>
      requires(ind < sizeof...(Tp)) &&
      std::is_constructible_v<alternative_t<ind>, Args&&...> constexpr void emplace(Args&&... args) noexcept(
          std::is_nothrow_constructible_v<alternative_t<ind>, Args&&...>) {
    reset();
    std::construct_at(std::addressof(get_impl<ind>(this->data_)), std::forward<Args>(args)...);
    this->index_ = ind;
  }

  template <typename Type, typename... Args>
  requires(exactly_once_v<Type>&& std::is_constructible_v<Type, Args&&...>) constexpr variant(
      in_place_type_t<Type>,
      Args&&... args) noexcept(noexcept(emplace<alternative_v<Type>>(std::forward<Args>(args)...))) {
    emplace<Type>(std::forward<Args>(args)...);
  }

  template <typename Type, typename... Args>
  requires(exactly_once_v<Type>&& std::is_constructible_v<Type, Args&&...>) constexpr void emplace(
      Args&&... args) noexcept(noexcept(emplace<alternative_v<Type>>(std::forward<Args>(args)...))) {
    emplace<alternative_v<Type>>(std::forward<Args>(args)...);
  }

  constexpr size_t index() const {
    return this->index_;
  }

  template <typename From, typename To = variant_ns::accepted_type_t<From&&, Tp...>>
  requires(exactly_once_v<To>&& std::is_constructible_v<To, From>) constexpr variant(From&& value) noexcept(
      std::is_nothrow_constructible_v<To, From>)
      : variant(in_place_index<alternative_v<To>>, std::forward<From>(value)) {}

  template <typename From, typename To = variant_ns::accepted_type_t<From&&, Tp...>>
  requires(exactly_once_v<To>&& std::is_assignable_v<To&, From>&& std::is_constructible_v<To, From>) constexpr variant&
  operator=(From&& value) noexcept(
      std::is_nothrow_assignable_v<To&, From>&& std::is_nothrow_constructible_v<To, From>) {
    if (alternative_v<To> == index()) {
      get<To>(*this) = std::forward<From>(value);
    } else {
      if constexpr (std::is_nothrow_constructible_v<To, From> || !std::is_nothrow_move_constructible_v<To>) {
        emplace<To>(std::forward<From>(value));
      } else {
        emplace<To>(To(std::forward<From>(value)));
      }
    }
    return *this;
  }

  template <typename Type>
  requires exactly_once_v<Type> friend constexpr bool holds_alternative(variant<Tp...> const& var) {
    return alternative_v<Type> == var.index_;
  }

  constexpr bool valueless_by_exception() const {
    return this->index_ == variant_npos;
  }

  friend constexpr bool operator<(variant const& lhs, variant const& rhs) {
    if (rhs.valueless_by_exception()) {
      return false;
    }
    if (lhs.valueless_by_exception()) {
      return true;
    }
    if (lhs.index() != rhs.index()) {
      return lhs.index() < rhs.index();
    }
    return visit(
        []<typename A, typename B>(A const& a, B const& b) -> bool {
          if constexpr (std::is_same_v<A, B>) {
            return a < b;
          }
          return false;
        },
        lhs, rhs);
  }
  friend constexpr bool operator>(variant const& lhs, variant const& rhs) {
    if (lhs.valueless_by_exception()) {
      return false;
    }
    if (rhs.valueless_by_exception()) {
      return true;
    }
    if (lhs.index() != rhs.index()) {
      return lhs.index() > rhs.index();
    }
    return visit(
        []<typename A, typename B>(A const& a, B const& b) -> bool {
          if constexpr (std::is_same_v<A, B>) {
            return a > b;
          }
          return false;
        },
        lhs, rhs);
  }
  friend constexpr bool operator==(variant const& lhs, variant const& rhs) {
    if (lhs.index() != rhs.index()) {
      return false;
    }
    if (lhs.index() == variant_npos) {
      return true;
    }
    return visit(
        []<typename A, typename B>(A const& a, B const& b) -> bool {
          if constexpr (std::is_same_v<A, B>) {
            return a == b;
          }
          return false;
        },
        lhs, rhs);
  }
  friend constexpr bool operator<=(variant const& lhs, variant const& rhs) {
    return !(lhs > rhs);
  }
  friend constexpr bool operator>=(variant const& lhs, variant const& rhs) {
    return !(lhs < rhs);
  }
  friend constexpr bool operator!=(variant const& lhs, variant const& rhs) {
    return !(lhs == rhs);
  }

  template <typename T, typename Variant>
  friend constexpr auto* get_if(Variant* var);
};

template <size_t ind, typename Variant>
constexpr auto&& get(Variant&& var) {
  if (ind != var.index()) {
    throw bad_variant_access();
  }
  if constexpr (!std::is_lvalue_reference_v<Variant>) {
    return std::move(get_impl<ind>(var.data_));
  } else {
    return get_impl<ind>(var.data_);
  }
}

template <size_t ind, typename Variant>
constexpr auto* get_if(Variant* var) {
  return ind != var->index() ? nullptr : std::addressof(get<ind>(*var));
}

template <typename T, typename Variant>
constexpr auto* get_if(Variant* var) {
  return Variant::template alternative_v<T> != var->index() ? nullptr : std::addressof(get<T>(*var));
}

namespace variant_ns {
// last level
template <typename F, size_t... Dimensions>
struct multi_array {
  constexpr const F& access() const {
    return data_;
  }
  F data_;
};

template <typename F, size_t DimSize, size_t... RestDimSize>
struct multi_array<F, DimSize, RestDimSize...> {
  template <typename... Args>
  constexpr const F& access(size_t first, Args... rest) const {
    return arr[first].access(rest...);
  }
  std::array<multi_array<F, RestDimSize...>, DimSize> arr;
};

template <typename... Vars>
struct variants_holder;

template <typename ArrayType, typename Variants, typename IndexSeq>
struct gen_vtable_impl;

template <typename R, typename Visitor, typename... Variants, size_t FirstDim, size_t... Dimensions, size_t... Inds>
struct gen_vtable_impl<multi_array<R (*)(Visitor, Variants...), FirstDim, Dimensions...>, variants_holder<Variants...>,
                       std::index_sequence<Inds...>> {
  using cur_variant_t = ind_to_type_t<sizeof...(Inds), std::remove_cvref_t<Variants>...>;
  using arr_t = multi_array<R (*)(Visitor, Variants...), FirstDim, Dimensions...>;

  template <size_t Ind, typename Arr>
  static constexpr Arr make_one() {
    return gen_vtable_impl<Arr, variants_holder<Variants...>, std::index_sequence<Inds..., Ind>>::make_table();
  }

  template <size_t... VariantInds>
  static constexpr arr_t make_many(std::index_sequence<VariantInds...>) {
    return arr_t{make_one<VariantInds, multi_array<R (*)(Visitor, Variants...), Dimensions...>>()...};
  }

  static constexpr arr_t make_table() {
    return make_many(std::make_index_sequence<variant_size_v<cur_variant_t>>{});
  }
};

// last level
template <typename R, typename Visitor, typename... Variants, size_t... Ind>
struct gen_vtable_impl<multi_array<R (*)(Visitor, Variants...)>, variants_holder<Variants...>,
                       std::index_sequence<Ind...>> {
  using arr_t = multi_array<R (*)(Visitor, Variants...)>;
  static constexpr decltype(auto) invoke(Visitor&& vis, Variants&&... vars) {
    return std::invoke(std::forward<Visitor>(vis), get<Ind>(std::forward<Variants>(vars))...);
  }
  static constexpr arr_t make_table() {
    return arr_t{&invoke};
  }
};

template <typename R, typename Visitor, typename... Variants>
struct gen_table {
  using func_ptr = R (*)(Visitor&&, Variants&&...);

  using arr_t = multi_array<func_ptr, variant_size_v<std::remove_reference_t<Variants>>...>;

  static constexpr arr_t table =
      gen_vtable_impl<arr_t, variants_holder<Variants...>, std::index_sequence<>>::make_table();
};
} // namespace variant_ns

template <typename Visitor, typename... Variants>
constexpr decltype(auto) visit(Visitor&& vis, Variants&&... vars) noexcept(false) {
  if ((vars.valueless_by_exception() || ...)) {
    throw bad_variant_access();
  }
  using ret_type = decltype(std::forward<Visitor>(vis)(get<0>(std::forward<Variants>(vars))...));
  constexpr auto& v_table = variant_ns::gen_table<ret_type, Visitor&&, Variants&&...>::table;
  auto func_ptr = v_table.access(vars.index()...);
  return (*func_ptr)(std::forward<Visitor>(vis), std::forward<Variants>(vars)...);
}
