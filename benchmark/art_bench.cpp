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

template <typename C> inline auto fill_container(C& c)
{
    using value_t = test::remove_key_const_t<typename C::value_type>;

    auto values = test::generate_values<value_t, max_values>(seed);
    for (const value_t& v : values) {
        c.insert(v);
    }

    return values;
}

template <typename T> inline void do_not_optimize(const T& value)
{
    benchmark::DoNotOptimize(test::value_of_value(value));
}

// Iteration (forward) through the tree
template <typename C> inline void fwditer(benchmark::State& state)
{
    using value_t = test::remove_key_const_t<typename C::value_type>;

    C container;

    // Fill the container
    for (value_t& v : test::generate_values<value_t, max_values>(seed)) {
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
    const auto values = fill_container(container);

    auto it = values.begin();
    for (auto _ : state) {
        // Remove
        state.PauseTiming();
        container.erase(test::key_of_value(*it));

        // Reinsert
        state.ResumeTiming();
        container.insert(*it);

        if (++it == values.end())
            it = values.begin();
    }
}

// Benchmark lookup of values in a container.
template <typename C> void lookup(benchmark::State& state)
{
    C container;
    const auto values = fill_container(container);

    auto it = values.begin();
    for (auto _ : state) {
        auto r = container.find(test::key_of_value(*it));
        do_not_optimize(*r);
        if (++it == values.end())
            it = values.begin();
    }
}

// Benchmark lookup of values in a full container, meaning that values
// are inserted in-order to take advantage of biased insertion, which
// yields a full tree.
template <typename C> void full_lookup(benchmark::State& state)
{
    using value_t = test::remove_key_const_t<typename C::value_type>;

    const auto values = test::generate_values<value_t, max_values>(seed);

    C container;
    {
        std::vector<value_t> sorted(values);
        std::sort(sorted.begin(), sorted.end());

        for (value_t& v : sorted) {
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
        state.ResumeTiming();
        auto pos = container.erase(test::key_of_value(*it));

        // Reinsert
        state.PauseTiming();
        container.insert(pos, *it);

        if (++it == values.end())
            it = values.begin();
    }
}

// Benchmark steady-state insert (into first half of range) and remove
// (from second second half of range), treating the container
// approximately like a queue with log-time access for all elements.
// This benchmark does not test the case where insertion and removal
// happen in the same region of the tree.  This benchmark counts two
// value constructors.
template <typename C> void queue_addrem(benchmark::State& state)
{
    // typedef typename std::remove_const<typename T::value_type>::type V;
    // typename KeyOfValue<typename T::key_type, V>::type key_of_value;

    // // Disable timing while we perform some initialization.
    // state.PauseTiming();
    // assert(max_values % 2 == 0);

    // T container;

    // const int half = max_values / 2;
    // vector<int> remove_keys(half);
    // vector<int> add_keys(half);

    // for (int i = 0; i < half; i++) {
    //     remove_keys[i] = i;
    //     add_keys[i] = i;
    // }

    // RandGen rand(FLAGS_test_random_seed);

    // random_shuffle(remove_keys.begin(), remove_keys.end(), rand);
    // random_shuffle(add_keys.begin(), add_keys.end(), rand);

    // Generator<V> g(max_values + FLAGS_benchmark_max_iters);

    // for (int i = 0; i < half; i++) {
    //     container.insert(g(add_keys[i]));
    //     container.insert(g(half + remove_keys[i]));
    // }

    // // There are three parts each of size "half":
    // // 1 is being deleted from  [offset - half, offset)
    // // 2 is standing            [offset, offset + half)
    // // 3 is being inserted into [offset + half, offset + 2 * half)
    // int offset = 0;

    for (auto _ : state) {
        // int idx = i % half;

        // if (idx == 0) {
        //     state.PauseTiming();
        //     random_shuffle(remove_keys.begin(), remove_keys.end(), rand);
        //     random_shuffle(add_keys.begin(), add_keys.end(), rand);
        //     offset += half;
        //     state.ResumeTiming();
        // }

        // int e = container.erase(key_of_value(g(offset - half + remove_keys[idx])));
        // assert(e == 1);
        // container.insert(g(offset + half + add_keys[idx]));
    }
}

// Mixed insertion and deletion in the same range using pre-constructed values.
template <typename C> void mixed_addrem(benchmark::State& state)
{
    // typedef typename std::remove_const<typename T::value_type>::type V;
    // typename KeyOfValue<typename T::key_type, V>::type key_of_value;

    // // Disable timing while we perform some initialization.
    // state.PauseTiming();
    // assert(max_values % 2 == 0);

    // T container;
    // RandGen rand(FLAGS_test_random_seed);

    // vector<V> values = GenerateValues<V>(max_values * 2);

    // // Create two random shuffles
    // vector<int> remove_keys(max_values);
    // vector<int> add_keys(max_values);

    // // Insert the first half of the values (already in random order)
    // for (int i = 0; i < max_values; i++) {
    //     container.insert(values[i]);

    //     // remove_keys and add_keys will be swapped before each round,
    //     // therefore fill add_keys here w/ the keys being inserted, so
    //     // they'll be the first to be removed.
    //     remove_keys[i] = i + max_values;
    //     add_keys[i] = i;
    // }

    for (auto _ : state) {
        // int idx = i % max_values;

        // if (idx == 0) {
        //     state.PauseTiming();
        //     remove_keys.swap(add_keys);
        //     random_shuffle(remove_keys.begin(), remove_keys.end(), rand);
        //     random_shuffle(add_keys.begin(), add_keys.end(), rand);
        //     state.ResumeTiming();
        // }

        // int e = container.erase(key_of_value(values[remove_keys[idx]]));
        // assert(e == 1);
        // container.insert(values[add_keys[idx]]);
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
    GENERATE_BENCH_SET(insert, Container, __VA_ARGS__);                                            \
/*    GENERATE_BENCH_SET(fwditer, Container, __VA_ARGS__);                                         \
GENERATE_BENCH_SET(lookup, Container, __VA_ARGS__);                                                \
GENERATE_BENCH_SET(full_lookup, Container, __VA_ARGS__);                                           \
GENERATE_BENCH_SET(insert, Container, __VA_ARGS__);                                                \
GENERATE_BENCH_SET(erase, Container, __VA_ARGS__);                                                 \
GENERATE_BENCH_SET(queue_addrem, Container, __VA_ARGS__);                                          \
GENERATE_BENCH_SET(mixed_addrem, Container, __VA_ARGS__);                                          \
GENERATE_BENCH_SET(fifo, Container, __VA_ARGS__)                                               \*/
/**/

GENERATE_BENCHMARKS(set<int>);
// GENERATE_BENCHMARKS(map<std::uint64_t, int>);
// GENERATE_BENCHMARKS(map<std::string, int>);
// GENERATE_BENCHMARKS(set<std::int64_t>);
// GENERATE_BENCHMARKS(map<std::int64_t, std::string>);
// GENERATE_BENCHMARKS(set<unsigned int>);
// GENERATE_BENCHMARKS(map<unsigned int, int>);
// GENERATE_BENCHMARKS(set<std::uint64_t>);
GENERATE_BENCHMARKS(map<std::uint64_t, std::string>);
// GENERATE_BENCHMARKS(set<int*>);
// GENERATE_BENCHMARKS(map<int*, int>);

BENCHMARK_MAIN();
