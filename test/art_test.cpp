#include "art/map.h"

#include <iostream>

int main()
{
    art::map<std::uint64_t, std::uint64_t> db;

    db.insert(std::make_pair(1818, 1));
    assert(!db.insert(std::make_pair(1818, 1)).second);

    db.insert(std::make_pair(1819, 2));

    db.insert(std::make_pair(181819, 3));

    for (int i = 0; i < 300; ++i)
        db.insert(std::make_pair(1820 + i, 4 + i));

    db.dump(std::cout);

    std::cout << "After erase:\n";
    for (int i = 0; i < 296; ++i)
        db.erase(1820 + i);

    db.erase(181819);

    db.dump(std::cout);

    std::cout << "Size: " << db.size() << std::endl;
    std::cout << "Iter. dist: " << std::distance(db.cbegin(), db.cend()) << std::endl;

    db.clear();

    return 0;
}
