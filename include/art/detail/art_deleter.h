#ifndef ART_DETAIL_ART_DELETER_HEADER_INCLUDED
#define ART_DETAIL_ART_DELETER_HEADER_INCLUDED

#include <memory>

namespace art
{
namespace detail
{

template <typename Node, typename Db> class node_deleter final
{
public:
    explicit node_deleter(Db& db_) noexcept
        : db{&db_}
    {
    }
    void operator()(Node* node) const noexcept { db->deallocate(node); }

private:
    Db* db;
};

template <typename Node, typename Db>
using unique_node_ptr = std::unique_ptr<Node, node_deleter<Node, Db>>;

} // namespace detail
} // namespace art

#endif // ART_DETAIL_ART_DELETER_HEADER_INCLUDED
