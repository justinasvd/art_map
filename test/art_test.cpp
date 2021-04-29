#include "art/map.h"

int main()
{
    art::map<std::uint64_t, std::uint64_t> db;

    db.insert(std::make_pair(5, 5));

    db.dump(std::cout);
    db.clear();

    return 0;
}
