#ifndef ART_DETAIL_BASIC_ART_POLICY_HEADER_INCLUDED
#define ART_DETAIL_BASIC_ART_POLICY_HEADER_INCLUDED

#include "node_type.h"

#include <memory>

namespace art
{
namespace detail
{

template <class Leaf, class Db> class basic_leaf_deleter
{
public:
    explicit basic_leaf_deleter(Db& db_) noexcept
        : db{&db_}
    {
    }
    void operator()(Leaf* leaf) const noexcept { db->deallocate(leaf); }

private:
    Db* db;
};

template <class Leaf, class Db>
using basic_db_leaf_unique_ptr = std::unique_ptr<Leaf, basic_leaf_deleter<Leaf, Db>>;

template <class NodePtr, class Db> struct basic_reclaim_db_node_ptr_at_scope_exit final {
    using leaf_type = typename NodePtr::leaf_type;
    using inode4_type = typename NodePtr::inode4_type;
    using inode16_type = typename NodePtr::inode16_type;
    using inode48_type = typename NodePtr::inode48_type;
    using inode256_type = typename NodePtr::inode256_type;
    // using leaf_reclaimable_ptr_type = leaf_reclaimable_ptr<header_type, Db, LeafReclamator>;
    // using db_leaf_reclamator_type = LeafReclamator<header_type, Db>;

    constexpr explicit basic_reclaim_db_node_ptr_at_scope_exit(
        NodePtr node_ptr_,
        Db& db_) noexcept // cppcheck-suppress constParameter
        : node_ptr{node_ptr_}
        , db{db_}
    {
    }

    ~basic_reclaim_db_node_ptr_at_scope_exit()
    {
        if (node_ptr == nullptr)
            return;

        switch (node_ptr.type()) {
        case node_type::LEAF: {
            // cppcheck-suppress unreadVariable
            // const auto leaf_unique_ptr =
            //     leaf_reclaimable_ptr_type{node_ptr.leaf, db_leaf_reclamator_type{db}};
            return;
        }
        case node_type::I4: {
            // TODO(laurynas): move inode accounting on destruction to unique_ptr
            // deleter too same as for leaves
            // cppcheck-suppress unreadVariable
            const auto node4_unique_ptr = std::unique_ptr<inode4_type>{node_ptr.node_4};
            return;
        }
        case node_type::I16: {
            // cppcheck-suppress unreadVariable
            const auto node16_unique_ptr = std::unique_ptr<inode16_type>{node_ptr.node_16};
            return;
        }
        case node_type::I48: {
            // cppcheck-suppress unreadVariable
            const auto node48_unique_ptr = std::unique_ptr<inode48_type>{node_ptr.node_48};
            return;
        }
        case node_type::I256: {
            // cppcheck-suppress unreadVariable
            const auto node256_unique_ptr = std::unique_ptr<inode256_type>{node_ptr.node_256};
            return;
        }
        }
        // CANNOT_HAPPEN(); // LCOV_EXCL_LINE
    }

    void delete_subtree() noexcept
    {
        if (node_ptr == nullptr || node_ptr.type() == node_type::LEAF)
            return;

        node_ptr.internal->delete_subtree(db);
    }

    // Non-copyable & non-movable
    basic_reclaim_db_node_ptr_at_scope_exit(const basic_reclaim_db_node_ptr_at_scope_exit&) =
        delete;
    basic_reclaim_db_node_ptr_at_scope_exit(basic_reclaim_db_node_ptr_at_scope_exit&&) = delete;
    basic_reclaim_db_node_ptr_at_scope_exit& operator=(
        const basic_reclaim_db_node_ptr_at_scope_exit&) = delete;
    basic_reclaim_db_node_ptr_at_scope_exit& operator=(basic_reclaim_db_node_ptr_at_scope_exit&&) =
        delete;

private:
    const NodePtr node_ptr;
    Db& db;
};

} // namespace detail
} // namespace art

#endif // ART_DETAIL_BASIC_ART_POLICY_HEADER_INCLUDED
