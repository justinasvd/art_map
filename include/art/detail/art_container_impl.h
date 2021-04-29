#ifndef ART_DETAIL_ART_CONTAINER_IMPL_HEADER_INCLUDED
#define ART_DETAIL_ART_CONTAINER_IMPL_HEADER_INCLUDED

#include "art_container.h"
#include "art_nodes.h"

#include <boost/config.hpp>

namespace art
{
namespace detail
{

template <typename P>
inline typename db<P>::const_iterator db<P>::internal_find(bitwise_key key) const noexcept
{
    if (empty())
        return end();

    node_ptr node{data.root};

    /*while (true) {
        const auto type = node.type();
        if (type == node_type::LEAF) {
            if (leaf_type::matches(node.leaf, remaining_key)) {
                const auto value = leaf::value(node.leaf);
                return value;
            }
            return {};
        }

        assert(ntype != detail::node_type::LEAF);

        const auto key_prefix_length = node.internal->key_prefix_length();
        if (node.internal->get_shared_key_prefix_length(remaining_key) < key_prefix_length)
            return {};
        remaining_key.shift_right(key_prefix_length);
        auto* const child = node.internal->find_child(ntype, remaining_key[0]).second;
        if (child == nullptr)
            return {};

        node = *child;
        remaining_key.shift_right(1);
    }*/
}

template <typename P>
inline typename db<P>::db_leaf_unique_ptr db<P>::make_leaf_ptr(bitwise_key key)
{
    // Allocate a single leaf
    leaf_allocator_type alloc(allocator());
    db_leaf_unique_ptr leaf_ptr(leaf_allocator_traits::allocate(alloc, 1),
                                basic_leaf_deleter<leaf_type, self_t>(*this));

    // Leaf storage construction is noexcept
    leaf_allocator_traits::construct(alloc, leaf_ptr.get(), key);

    increment_leaf_count();
    return leaf_ptr;
}

template <typename P>
template <typename... Args>
inline typename db<P>::db_leaf_unique_ptr db<P>::make_leaf_ptr(bitwise_key key, Args&&... args)
{
    auto leaf = make_leaf_ptr(key);
    // Emplace the value into the leaf
    leaf->emplace_value(allocator(), std::forward<Args>(args)...);
    return leaf;
}

template <typename P> inline void db<P>::deallocate(leaf_type* leaf) noexcept
{
    leaf->destroy_value(allocator());

    leaf_allocator_type alloc(allocator());
    leaf_allocator_traits::deallocate(alloc, leaf, 1);

    decrement_leaf_count();
}

template <typename P>
template <typename... Args>
inline typename db<P>::iterator db<P>::emplace_key_args(std::true_type, fast_key_type key,
                                                        Args&&... args)
{
    // Emplacement support for multiset/multimap
    if (!empty()) {
        // return internal_emplace(upper_bound(key), std::forward<Args>(args)...);
    } else {
        data.root = node_ptr{make_leaf_ptr(key, std::forward<Args>(args)...).release()};
        return iterator(data.root, 0);
    }
}

// Inserts a value into the tree only if it does not already exist. The
// boolean return value indicates whether insertion succeeded or failed.
template <typename P>
template <typename... Args>
inline std::pair<typename db<P>::iterator, bool> db<P>::emplace_key_args(std::false_type,
                                                                         fast_key_type key,
                                                                         Args&&... args)
{
    // Emplacement support for set/map
    if (!empty()) {
        /*auto pos = internal_locate(key);
        if (pos != nullptr && compare) {
            return std::make_pair(pos, false);
        } else {
            return std::make_pair(internal_emplace(), true);
        }*/
    } else {
        data.root = node_ptr{make_leaf_ptr(key, std::forward<Args>(args)...).release()};
        return std::make_pair(iterator(data.root, 0), true);
    }
}

template <typename P> inline typename db<P>::size_type db<P>::erase(fast_key_type key)
{
    if (empty())
        return 0;

    /*const auto k = bitwise_key{key};

    if (data.root.type() == node_type::LEAF) {
        if (leaf_type::matches(data.root.leaf, k)) {
            // // FIXME(laurynas): why not same in node deleters?
            // const auto delete_root_leaf_on_scope_exit{
            //     art_policy::make_db_reclaimable_leaf_ptr(data.root.leaf, *this)};
            data.root = node_ptr{nullptr};
            return 1;
        }
        return 0;
    }

    auto* node = &data.root;
    tree_depth depth{k.size()};
    bitwise_key remaining_key{k};

    while (true) {
        const auto node_type = node->type();
        assert(node_type != node_type::LEAF);
        assert(depth < k.size());

        const auto key_prefix_length = node->internal->key_prefix_length();
        const auto shared_prefix_len = node->internal->get_shared_key_prefix_length(remaining_key);
        if (shared_prefix_len < key_prefix_length)
            return 0;

        assert(shared_prefix_len == key_prefix_length);
        depth += key_prefix_length;
        remaining_key.shift_right(key_prefix_length);

        const auto [child_i, child_ptr] = node->internal->find_child(node_type, remaining_key[0]);

        if (child_ptr == nullptr)
            return 0;

        if (child_ptr->type() == node_type::LEAF) {
            if (!leaf_type::matches(child_ptr->leaf, k))
                return 0;

            const auto is_node_min_size = node->internal->is_min_size();

            if (BOOST_LIKELY(!is_node_min_size)) {
                node->internal->remove(child_i, *this);
                return 1;
            }

            assert(is_node_min_size);

            if (node_type == node_type::I4) {
                std::unique_ptr<inode_4> current_node{node->node_4};
                *node = current_node->leave_last_child(child_i, *this);
                decrease_memory_use(sizeof(inode_4));

                assert(inode4_count_ > 0);
                --inode4_count_;
                ++deleted_inode4_count_;
                assert(deleted_inode4_count_ <= created_inode4_count_);

            } else if (node_type == node_type::I16) {
                std::unique_ptr<inode_16> current_node{node->node_16};
                auto new_node{inode_4::create(std::move(current_node), child_i, *this)};
                *node = node_ptr(new_node.release());
                decrease_memory_use(sizeof(inode_16) - sizeof(inode_4));

                assert(inode16_count_ > 0);
                --inode16_count_;
                ++inode4_count_;
                ++inode16_to_inode4_count_;
                assert(inode16_to_inode4_count_ <= inode4_to_inode16_count_);

            } else if (node_type == node_type::I48) {
                std::unique_ptr<inode_48> current_node{node->node_48};
                auto new_node{inode_16::create(std::move(current_node), child_i, *this)};
                *node = node_ptr(new_node.release());
                decrease_memory_use(sizeof(inode_48) - sizeof(inode_16));

                assert(inode48_count_ > 0);
                --inode48_count_;
                ++inode16_count_;
                ++inode48_to_inode16_count_;
                assert(inode48_to_inode16_count_ <= inode16_to_inode48_count_);

            } else {
                assert(node_type == node_type::I256);
                std::unique_ptr<inode_256> current_node{node->node_256};
                auto new_node{inode_48::create(std::move(current_node), child_i, *this)};
                *node = node_ptr(new_node.release());
                decrease_memory_use(sizeof(inode_256) - sizeof(inode_48));

                assert(inode256_count_ > 0);
                --inode256_count_;
                ++inode48_count_;
                ++inode256_to_inode48_count_;
                assert(inode256_to_inode48_count_ <= inode48_to_inode256_count_);
            }

            return 1;
        }

        node = reinterpret_cast<node_ptr*>(child_ptr);
        ++depth;
        remaining_key.shift_right(1);
    }*/
    return 0;
}

template <typename P> inline typename db<P>::iterator db<P>::erase(iterator first, iterator last)
{
    while (first != last)
        first = erase(first);
    return first;
}

template <typename P> inline void db<P>::swap(self_t& other) noexcept
{
    // Swap the tree
    std::swap(allocator(), other.allocator());
    std::swap(data.root, other.data.root);

    // Swap the stats
    std::swap(current_memory_use_, other.current_memory_use_);

    std::swap(leaf_count_, other.leaf_count_);
    std::swap(inode4_count_, other.inode4_count_);
    std::swap(inode16_count_, other.inode16_count_);
    std::swap(inode48_count_, other.inode48_count_);
    std::swap(inode256_count_, other.inode256_count_);

    std::swap(created_inode4_count_, other.created_inode4_count_);
    std::swap(inode4_to_inode16_count_, other.inode4_to_inode16_count_);
    std::swap(inode16_to_inode48_count_, other.inode16_to_inode48_count_);
    std::swap(inode48_to_inode256_count_, other.inode48_to_inode256_count_);

    std::swap(deleted_inode4_count_, other.deleted_inode4_count_);
    std::swap(inode16_to_inode4_count_, other.inode16_to_inode4_count_);
    std::swap(inode48_to_inode16_count_, other.inode48_to_inode16_count_);
    std::swap(inode256_to_inode48_count_, other.inode256_to_inode48_count_);

    std::swap(key_prefix_splits_, other.key_prefix_splits_);
}

template <typename P> inline void db<P>::clear()
{
    if (data.root != nullptr) {
        delete_subtree(data.root);

        // It is possible to reset the counter to zero instead of decrementing it for
        // each leaf, but not sure the savings will be significant.
        assert(leaf_count_ == 0);

        data.root = node_ptr{nullptr};
        current_memory_use_ = 0;
        inode4_count_ = 0;
        inode16_count_ = 0;
        inode48_count_ = 0;
        inode256_count_ = 0;
    }
}

template <typename NodePtr> inline void dump_node(std::ostream& os, NodePtr node)
{
    os << "node at: " << node.leaf;
    if (node.leaf == nullptr) {
        os << '\n';
        return;
    }
    os << ", type = ";
    switch (node.type()) {
    case node_type::LEAF:
        node.leaf->dump(os);
        break;
    case node_type::I4:
    case node_type::I16:
    case node_type::I48:
    case node_type::I256:
        node.internal->dump(os);
        break;
    }
}

template <typename P> inline void db<P>::dump(std::ostream& os) const
{
    os << "db dump, current memory use = " << current_memory_use() << '\n';
    dump_node(os, data.root);
}

} // namespace detail
} // namespace art

#endif // ART_DETAIL_ART_CONTAINER_IMPL_HEADER_INCLUDED
