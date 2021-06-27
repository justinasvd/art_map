#include "art/map.h"

#include "test_utils.hpp"

#include <iostream>

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

int main()
{
    using key_type = std::uint64_t;
    using value_type = std::uint64_t;

    art::map<key_type, value_type, std::less<key_type>> db;

    std::cout << "Inserting...\n";
    fill_container(db);

    std::cout << "Inserted " << db.size() << " elements, on average "
              << (static_cast<double>(db.current_memory_use()) / db.size())
              << " bytes per element\n";

    /*std::cout << "Erasing...\n";
    for (key_type i = 4; i < max_keys; ++i) {
        std::size_t cnt = db.erase(1818 + i);
        assert(cnt == 1);
    }
    // db.erase(std::numeric_limits<key_type>::max());

    p = db.emplace(1825, max_keys);

    db.dump(std::cout);

    std::cout << "Size: " << db.size() << std::endl;
    std::cout << "Iter. dist: " << std::distance(db.cbegin(), db.cend()) << std::endl;

    for (const auto& p : db) {
        std::cout << p.first << ": " << p.second << std::endl;
    }

    auto it = db.lower_bound(1822);
    std::cout << "Lower bound: " << it->first << ": " << it->second << std::endl;
    it = db.upper_bound(1822);
    std::cout << "Upper bound: " << it->first << ": " << it->second << std::endl;*/

    db.clear();

    return 0;
}
