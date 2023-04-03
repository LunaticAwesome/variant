#pragma once

#include <concepts>

inline constexpr size_t variant_npos = -1;

namespace variant_ns {
template <typename T, typename... Tp>
struct get_first {
  using type = T;
};

template <typename... Tp>
using get_first_t = typename get_first<Tp...>::type;

template <bool... Bools>
struct count_of_bools {
  static constexpr size_t value = 0;
};

template <bool B, bool... Bools>
struct count_of_bools<B, Bools...> {
  static constexpr size_t value = B + count_of_bools<Bools...>::value;
};

template <bool... Bools>
inline constexpr size_t count_of_bools_v = count_of_bools<Bools...>::value;

template <template <typename> typename P, typename... Tp>
inline constexpr bool all_of_v = count_of_bools_v<P<Tp>::value...> == sizeof...(Tp);

template <typename Type, typename... Tp>
inline constexpr bool exactly_once_v = count_of_bools_v<std::is_same_v<Type, Tp>...> == 1;

template <size_t ind, typename... Tp>
requires(ind < sizeof...(Tp)) struct ind_to_type;

template <size_t ind, typename T, typename... Tp>
struct ind_to_type<ind, T, Tp...> : ind_to_type<ind - 1, Tp...> {};

template <typename T, typename... Tp>
struct ind_to_type<0, T, Tp...> {
  using type = T;
};

template <size_t ind, typename... Tp>
using ind_to_type_t = typename ind_to_type<ind, Tp...>::type;

template <typename Type, typename... Tp>
struct type_to_ind : std::integral_constant<size_t, 1> {};

template <typename Type, typename T, typename... Tp>
struct type_to_ind<Type, T, Tp...> {
  static constexpr size_t value = type_to_ind<Type, Tp...>::value + 1;
};

template <typename T, typename... Tp>
struct type_to_ind<T, T, Tp...> : std::integral_constant<size_t, 0> {};

template <typename Type, typename... Tp>
inline constexpr size_t type_to_ind_v = type_to_ind<Type, Tp...>::value;

template <typename Type, typename T>
concept ValidConversion = requires(Type x, T&& t) {
  x = {std::forward<T>(t)};
};

template <std::size_t ind, typename TypeCheck, typename... Tp>
struct builder_fn {
  static constexpr std::integral_constant<std::size_t, ind> foo();
};

template <std::size_t ind, typename TypeCheck, typename T, typename... Tp>
struct builder_fn<ind, TypeCheck, T, Tp...> : builder_fn<ind + 1, TypeCheck, Tp...> {
  using builder_fn<ind + 1, TypeCheck, Tp...>::foo;

  static constexpr std::integral_constant<std::size_t, ind> foo(T) requires ValidConversion<T, TypeCheck>;
};

template <typename T, typename... Tp>
concept Callable = requires(T && x, builder_fn<0, T, Tp...> a) {
  builder_fn<0, T, Tp...>::foo(std::forward<T>(x));
};

template <typename... Tp>
struct accepted_index : std::integral_constant<size_t, variant_npos> {};

template <typename T, typename... Tp>
requires Callable<T, Tp...> struct accepted_index<T, Tp...>
    : decltype(builder_fn<0, T, Tp...>::foo(std::declval<T>())) {};

template <typename T, typename... Tp>
inline constexpr size_t accepted_index_v = accepted_index<T, Tp...>::value;

template <typename T, typename... Tp>
using accepted_type_t = ind_to_type_t<accepted_index_v<T, Tp...>, Tp...>;
} // namespace variant_ns
