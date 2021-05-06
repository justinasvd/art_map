#include "art/map.h"

int main()
{
    art::map<std::uint64_t, std::uint64_t> db;

    db.insert(std::make_pair(1818, 5));
    assert(!db.insert(std::make_pair(1818, 5)).second);

    db.insert(std::make_pair(1819, 5));

    db.dump(std::cout);
    db.clear();

    return 0;
}
