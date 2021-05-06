#include "art/map.h"

int main()
{
    art::map<std::uint64_t, std::uint64_t> db;

    db.insert(std::make_pair(1818, 1));
    assert(!db.insert(std::make_pair(1818, 1)).second);

    db.insert(std::make_pair(1819, 2));

    db.insert(std::make_pair(181819, 3));

    db.insert(std::make_pair(1820, 4));
    db.insert(std::make_pair(1821, 5));

    db.insert(std::make_pair(1822, 6));

    db.dump(std::cout);
    db.clear();

    return 0;
}
