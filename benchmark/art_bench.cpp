// Copyright 2021 Justinas V. Daugmaudis

#include <benchmark/benchmark.h>

#include <map>
#include <set>

#include "art/map.h"
#include "art/set.h"

#include "test/test_utils.hpp"

// The number of values to use for benchmarks
static constexpr unsigned int max_values = 1000000;

// Seed for the sample generators
static constexpr unsigned int seed = 123456789;

template <typename T> struct precomputed_dataset {
    static const std::vector<T> values;
    static std::vector<T> generate() { return test::generate_values<T, max_values>(seed); }
};

// Precompute static dataset for different value types
template <typename T>
const std::vector<T> precomputed_dataset<T>::values = precomputed_dataset<T>::generate();

template <typename C> [[nodiscard]] inline auto bench_dataset()
{
    using value_type = test::remove_key_const_t<typename C::value_type>;
    return precomputed_dataset<value_type>::values;
}

template <typename C> [[nodiscard]] inline auto fill_container(C& c)
{
    auto values = bench_dataset<C>();
    c.insert(values.begin(), values.end());
    return values;
}

template <typename C, typename V> inline void fill_container(C& c, V&& values)
{
    for (auto& v : values) {
        c.insert(std::move(v));
    }
}

template <typename T> inline void do_not_optimize(const T& value)
{
    benchmark::DoNotOptimize(test::value_of_value(value));
}

// Iteration (forward) through the tree
template <typename C> inline void fwditer(benchmark::State& state)
{
    C container;

    // Fill the container
    fill_container(container, bench_dataset<C>());

    auto it = container.begin();

    for (auto _ : state) {
        do_not_optimize(*it);
        if (++it == container.end())
            it = container.begin();
    }
}

// Benchmark insertion of values into a container.
template <typename C> inline void insert(benchmark::State& state)
{
    C container;

    const auto values = bench_dataset<C>();

    auto it = values.begin();
    for (auto _ : state) {
        auto r = container.insert(*it);
        do_not_optimize(&r);

        // Clear the container on wrap
        if (++it == values.end()) {
            state.PauseTiming();
            it = values.begin();
            container.clear();
            state.ResumeTiming();
        }
    }
}

// Benchmark lookup of values in a container.
template <typename C> void find(benchmark::State& state)
{
    C container;
    const auto values = fill_container(container);

    auto it = values.begin();
    for (auto _ : state) {
        auto r = container.find(test::key_of_value(*it));
        assert(r != container.end());
        assert(test::key_of_value(*r) == test::key_of_value(*it));
        do_not_optimize(*r);
        if (++it == values.end())
            it = values.begin();
    }
}

// Benchmark lookup of values in a container, where values
// are inserted in-order.
template <typename C> void find_sorted(benchmark::State& state)
{
    const auto values = bench_dataset<C>();

    C container;
    {
        auto sorted = values; // Make a copy
        std::sort(sorted.begin(), sorted.end());
        fill_container(container, std::move(sorted));
    }

    auto it = values.begin();
    for (auto _ : state) {
        auto r = container.find(test::key_of_value(*it));
        do_not_optimize(*r);
        if (++it == values.end())
            it = values.begin();
    }
}

// Benchmark deletion of values from a container.
template <typename C> inline void erase(benchmark::State& state)
{
    C container;
    const auto values = fill_container(container);

    auto it = values.begin();
    for (auto _ : state) {
        // Remove
        std::size_t count = container.erase(test::key_of_value(*it));
        assert(count == 1);
        do_not_optimize(count);

        // Reinsert values
        if (++it == values.end()) {
            state.PauseTiming();
            it = values.begin();
            assert(container.empty());
            container.insert(values.begin(), values.end());
            state.ResumeTiming();
        }
    }
}

// Sequentialy erases the container
template <typename C> inline void erase_sequential(benchmark::State& state)
{
    C container;
    const auto values = fill_container(container);

    auto it = container.begin();
    for (auto _ : state) {
        // Remove
        it = container.erase(it);
        do_not_optimize(&it);

        // Reinsert values
        if (it == container.end()) {
            state.PauseTiming();
            assert(container.empty());
            container.insert(values.begin(), values.end());
            it = container.begin();
            state.ResumeTiming();
        }
    }
}

#define GENERATE_BENCH_SET(TestName, ...)                                                          \
    BENCHMARK_TEMPLATE(TestName, std::__VA_ARGS__);                                                \
    BENCHMARK_TEMPLATE(TestName, art::__VA_ARGS__);                                                \
    BENCHMARK_TEMPLATE(TestName, std::multi##__VA_ARGS__);                                         \
    BENCHMARK_TEMPLATE(TestName, art::multi##__VA_ARGS__)                                          \
    /**/

#define GENERATE_BENCHMARKS(...)                                                                   \
    GENERATE_BENCH_SET(fwditer, __VA_ARGS__);                                                      \
    GENERATE_BENCH_SET(find, __VA_ARGS__);                                                         \
    GENERATE_BENCH_SET(find_sorted, __VA_ARGS__);                                                  \
    GENERATE_BENCH_SET(insert, __VA_ARGS__);                                                       \
    GENERATE_BENCH_SET(erase, __VA_ARGS__);                                                        \
    GENERATE_BENCH_SET(erase_sequential, __VA_ARGS__)                                              \
    /**/

GENERATE_BENCHMARKS(set<int>);
GENERATE_BENCHMARKS(map<int, std::string>);
GENERATE_BENCHMARKS(set<int*>);
GENERATE_BENCHMARKS(map<int*, std::string>);
GENERATE_BENCHMARKS(set<std::uint64_t>);
GENERATE_BENCHMARKS(map<std::uint64_t, std::string>);

BENCHMARK_MAIN();
