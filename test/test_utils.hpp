// Copyright 2021 Justinas V. Daugmaudis
#ifndef HEADER_TEST_UTILS_HPP
#define HEADER_TEST_UTILS_HPP

#include <random>
#include <set>
#include <string>
#include <vector>

namespace test
{

namespace detail
{

template <typename T> struct caster {
    [[nodiscard]] static T apply(unsigned int m) noexcept { return static_cast<T>(m); }
};

template <typename T> struct caster<T*> {
    [[nodiscard]] static T* apply(unsigned int m) noexcept { return reinterpret_cast<T*>(m << 3); }
};

// Identity generator
template <typename T> struct generator {
    explicit constexpr generator(unsigned int m) noexcept
        : max_value(m)
    {
    }

    [[nodiscard]] T operator()(unsigned int i) const noexcept
    {
        assert(i <= max_value);
        return caster<T>::apply(i);
    }

private:
    unsigned int max_value;
};

// String generator
[[nodiscard]] inline constexpr std::size_t num_digits(std::size_t n) noexcept
{
    return n < 10 ? 1 : num_digits(n / 10) + 1;
}

template <> struct generator<std::string> {
    explicit constexpr generator(unsigned int m) noexcept
        : max_digits(num_digits(m))
    {
    }

    // Generates strings of minimum length `max_digits`.
    // Appends leading zeros if the number of digits of `i` is smaller
    [[nodiscard]] std::string operator()(unsigned int i) const
    {
        const unsigned int digits = num_digits(i);
        return std::string(max_digits > digits ? max_digits - digits : 0, '0')
            .append(std::to_string(i));
    }

private:
    unsigned int max_digits;
};

// Pair generator
template <typename T, typename U> struct generator<std::pair<T, U>> {
    explicit constexpr generator(unsigned int m)
        : tgen(m)
        , ugen(m)
    {
    }

    [[nodiscard]] std::pair<T, U> operator()(unsigned int i) const
    {
        return std::make_pair(tgen(i), ugen(i));
    }

private:
    generator<std::remove_const_t<T>> tgen;
    generator<std::remove_const_t<U>> ugen;
};

template <typename T> struct remove_key_const : std::remove_const<T> {
};

template <typename U, typename V> struct remove_key_const<std::pair<U, V>> {
    using type = std::pair<std::remove_const_t<U>, V>;
};

// Identity function
template <typename T> struct key_of_value {
    static const T& get(const T& value) noexcept { return value; }
};

template <typename U, typename V> struct key_of_value<std::pair<U, V>> {
    static const U& get(const std::pair<U, V>& value) noexcept { return value.first; }
};

// Identity function
template <typename T> struct value_of_value {
    static const T& get(const T& value) noexcept { return value; }
};

template <typename U, typename V> struct value_of_value<std::pair<U, V>> {
    static const V& get(const std::pair<U, V>& value) noexcept { return value.second; }
};

} // namespace detail

template <typename T> using remove_key_const_t = typename detail::remove_key_const<T>::type;

// Generate n values for our tests and benchmarks. Value range is [0, maxval].
[[nodiscard]] inline std::vector<unsigned int> generate_numbers(unsigned int seed, unsigned int n,
                                                                unsigned int max_value)
{
    std::vector<unsigned int> values;
    std::set<unsigned int> unique_values;

    values.reserve(n);

    std::mt19937 gen(seed);
    std::uniform_int_distribution<unsigned int> dist(0, max_value);

    while (values.size() < n) {

        unsigned int value;
        do {
            value = dist(gen);
        } while (unique_values.find(value) != unique_values.end());

        values.push_back(value);
        unique_values.insert(value);
    }

    return values;
}

// Generates values in the range [0, 4 * N]
template <typename T, unsigned int N>
[[nodiscard]] inline std::vector<T> generate_values(unsigned int seed)
{
    static_assert(N != 0, "Generated set of values cannot be empty");

    static constexpr unsigned int upper_bound = 4 * N;
    static_assert(upper_bound > N, "Bound overflow");

    std::vector<unsigned int> nums = generate_numbers(seed, N, upper_bound);
    detail::generator<T> gen(upper_bound);

    std::vector<T> vec;
    vec.reserve(N);

    for (unsigned int n : nums) {
        vec.push_back(gen(n));
    }

    return vec;
}

// Should not take as a parameter a temporary, since this function returns
// a reference to the key, instead of making a full copy
template <typename T> [[nodiscard]] inline auto key_of_value(const T& value) noexcept
{
    return detail::key_of_value<T>::get(value);
}

// Should not take as a parameter a temporary, since this function returns
// a reference to the key, instead of making a full copy
template <typename T> [[nodiscard]] inline auto value_of_value(const T& value) noexcept
{
    return detail::value_of_value<T>::get(value);
}

} // namespace test

#endif // HEADER_TEST_UTILS_HPP
