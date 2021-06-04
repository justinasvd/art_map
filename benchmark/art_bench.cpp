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

template <typename C> inline auto bench_dataset()
{
    using value_t = test::remove_key_const_t<typename C::value_type>;
    return test::generate_values<value_t, max_values>(seed);
}

template <typename C> inline auto fill_container(C& c)
{
    auto values = bench_dataset<C>();
    c.insert(values.begin(), values.end());
    return values;
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
    for (auto& v : bench_dataset<C>()) {
        container.insert(std::move(v));
    }

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

        for (auto& v : sorted) {
            container.insert(std::move(v));
        }
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

// Insertion at end, removal from the beginning. This benchmark
// counts two value constructors.
template <typename C> inline void fifo(benchmark::State& state)
{
    C container;
    const auto values = fill_container(container);

    auto it = values.begin();
    for (auto _ : state) {
        container.erase(container.begin());
        container.insert(container.end(), *it);
        if (++it == values.end())
            it = values.begin();
    }
}

#define GENERATE_BENCH_FUNCTION(TestName, Container) BENCHMARK_TEMPLATE(TestName, Container)

#define MERGE_TOKENS(T, ...) T __VA_OPT__(, ) __VA_ARGS__

#define GENERATE_BENCH_SET(TestName, Container, ...)                                               \
    GENERATE_BENCH_FUNCTION(TestName, MERGE_TOKENS(std::Container, __VA_ARGS__));                  \
    GENERATE_BENCH_FUNCTION(TestName, MERGE_TOKENS(std::multi##Container, __VA_ARGS__));           \
    GENERATE_BENCH_FUNCTION(TestName, MERGE_TOKENS(art::Container, __VA_ARGS__));                  \
    GENERATE_BENCH_FUNCTION(TestName, MERGE_TOKENS(art::multi##Container, __VA_ARGS__))            \
    /**/

#define GENERATE_BENCHMARKS(Container, ...)                                                        \
    GENERATE_BENCH_SET(fwditer, Container, __VA_ARGS__);                                           \
    GENERATE_BENCH_SET(find, Container, __VA_ARGS__);                                              \
    GENERATE_BENCH_SET(find_sorted, Container, __VA_ARGS__);                                       \
    GENERATE_BENCH_SET(insert, Container, __VA_ARGS__);                                            \
    GENERATE_BENCH_SET(erase, Container, __VA_ARGS__);                                             \
    GENERATE_BENCH_SET(fifo, Container, __VA_ARGS__)                                               \
    /**/

GENERATE_BENCHMARKS(set<int>);
GENERATE_BENCHMARKS(map<int, std::string>);
// GENERATE_BENCHMARKS(set<int*>);
// GENERATE_BENCHMARKS(map<int*, std::string>);
GENERATE_BENCHMARKS(set<std::uint64_t>);
GENERATE_BENCHMARKS(map<std::uint64_t, std::string>);

BENCHMARK_MAIN();
