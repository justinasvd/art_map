#include "art/map.h"

#include <iostream>

int main()
{
    using key_type = std::uint64_t;
    using value_type = std::uint64_t;

    static constexpr key_type max_keys = 50000000;

    art::map<key_type, value_type, std::less<key_type>> db;

    for (key_type i = 0; i < max_keys; ++i) {
        const key_type key = 1818 + i;
        const value_type value = 1 + i;

        auto p = db.emplace(key, value);
        assert(p.second);
        assert(p.first->first == key && p.first->second == value);
        p = db.insert(std::make_pair(key, value));
        assert(!p.second);
    }

    db.emplace(181819, 1);

    std::cout << "After erase:\n";
    for (key_type i = 4; i < max_keys; ++i) {
        std::size_t cnt = db.erase(1818 + i);
        assert(cnt == 1);
    }

    // db.erase(181819);

    db.dump(std::cout);

    std::cout << "Size: " << db.size() << std::endl;
    std::cout << "Iter. dist: " << std::distance(db.cbegin(), db.cend()) << std::endl;

    for (const auto& p : db) {
        std::cout << p.first << ": " << p.second << std::endl;
    }

    db.clear();

    return 0;
}
