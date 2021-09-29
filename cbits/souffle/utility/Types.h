/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file Types.h
 *
 * @brief Shared type definitions
 *
 ***********************************************************************/

#pragma once

#include <memory>
#include <type_traits>
#include <vector>

namespace souffle {
template <typename A>
using Own = std::unique_ptr<A>;

template <typename A, typename B = A, typename... Args>
Own<A> mk(Args&&... xs) {
    return std::make_unique<B>(std::forward<Args>(xs)...);
}

template <typename A>
using VecOwn = std::vector<Own<A>>;

/**
 * Copy the const qualifier of type T onto type U
 */
template <typename A, typename B>
using copy_const = std::conditional<std::is_const_v<A>, const B, B>;

template <typename A, typename B>
using copy_const_t = typename copy_const<A, B>::type;

namespace detail {
template <typename T, typename U = void>
struct is_range_impl : std::false_type {};

template <typename T>
struct is_range_impl<T, std::void_t<decltype(*std::begin(std::declval<T&>()))>> : std::true_type {};

}  // namespace detail

/**
 * A simple test to check if T is a range (i.e. has std::begin())
 */
template <typename T>
struct is_range : detail::is_range_impl<T> {};

template <typename T>
inline constexpr bool is_range_v = is_range<T>::value;

/**
 * Type identity, remove once we have C++20
 */
template <typename T>
struct type_identity {
    using type = T;
};

/**
 * Remove cv ref, remove once we have C++ 20
 */
template <typename T>
using remove_cvref = std::remove_cv<std::remove_reference_t<T>>;

template <class T>
using remove_cvref_t = typename remove_cvref<T>::type;

template <typename T>
struct is_pointer_like : std::is_pointer<T> {};

template <typename T>
struct is_pointer_like<Own<T>> : std::true_type {};

template <typename T>
inline constexpr bool is_pointer_like_v = is_pointer_like<T>::value;

}  // namespace souffle
