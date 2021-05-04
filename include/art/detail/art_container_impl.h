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
inline typename db<P>::const_iterator db<P>::internal_locate(bitwise_key& key) const noexcept
{
    using inode = basic_inode_impl<self_t>;

    const_iterator pos(data.root, 0);

    if (pos) {
        while (pos.type() != node_type::LEAF) {
            const unsigned prefix_length = pos.prefix_length();
            if (pos.shared_prefix_length(key) < prefix_length)
                break;
            key.shift_right(prefix_length);

            const auto child = static_cast<inode*>(pos.node)->find_child(key.front());
            if (!child)
                break;
            pos = child;

            // We have consumed one byte during the child lookup
            key.shift_right(1);
        }
    }

    return pos;
}

template <typename P>
inline typename db<P>::const_iterator db<P>::internal_find(fast_key_type key) const noexcept
{
    bitwise_key bitk(key);
    auto pos = internal_locate(bitk);
    return pos.match(bitk) ? pos : end();
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
inline typename db<P>::iterator db<P>::internal_emplace(const_iterator hint, bitwise_key key,
                                                        Args&&... args)
{
    // Preemptively construct a leaf
    auto leaf_ptr = make_leaf_ptr(key, std::forward<Args>(args)...);

    node_ptr dst = hint ? hint.node : data.root;

    if (BOOST_UNLIKELY(dst == nullptr)) {
        data.root = node_ptr(leaf_ptr.release());
        return iterator(data.root, 0);
    }

    if (dst->type() == node_type::LEAF) {
        // Can only happen in multivalued container case
        /*const auto existing_key = leaf::key(node->leaf);
        if (unlikely(k == existing_key))
            return false;

        auto leaf = art_policy::make_db_leaf_ptr(k, v, *this);
        increase_memory_use(sizeof(detail::inode_4));
        // TODO(laurynas): try to pass leaf node type instead of generic node
        // below. This way it would be apparent that its key prefix does not need
        // updating as leaves don't have any.
        auto new_node =
            detail::inode_4::create(existing_key, remaining_key, depth, *node, std::move(leaf));
        *node = detail::node_ptr{new_node.release()};
        ++inode4_count;
        ++created_inode4_count;
        assert(created_inode4_count >= inode4_count);
        return true;*/
    }

    /*if (BOOST_LIKELY(!dst.internal->is_full())) {
        dst.internal->add(std::move(leaf_ptr), depth);
    } else {
    }


    while (true) {

        assert(node_type != detail::node_type::LEAF);
        assert(depth < detail::art_key::size);

        const auto key_prefix_length = node->internal->key_prefix_length();
        const auto shared_prefix_len = node->internal->get_shared_key_prefix_length(remaining_key);
        if (shared_prefix_len < key_prefix_length) {
            auto leaf = art_policy::make_db_leaf_ptr(k, v, *this);
            increase_memory_use(sizeof(detail::inode_4));
            auto new_node =
                detail::inode_4::create(*node, shared_prefix_len, depth, std::move(leaf));
            *node = detail::node_ptr{new_node.release()};
            ++inode4_count;
            ++created_inode4_count;
            ++key_prefix_splits;
            assert(created_inode4_count >= inode4_count);
            assert(created_inode4_count > key_prefix_splits);
            return true;
        }

        assert(shared_prefix_len == key_prefix_length);
        depth += key_prefix_length;
        remaining_key.shift_right(key_prefix_length);

        auto* const child = node->internal->find_child(node_type, remaining_key[0]).second;

        if (child == nullptr) {
            auto leaf = art_policy::make_db_leaf_ptr(k, v, *this);

            const auto node_is_full = node->internal->is_full();

            if (likely(!node_is_full)) {
                node->internal->add(std::move(leaf), depth);
                return true;
            }

            assert(node_is_full);

            if (node_type == detail::node_type::I4) {
                assert(inode4_count > 0);

                increase_memory_use(sizeof(detail::inode_16) - sizeof(detail::inode_4));
                std::unique_ptr<detail::inode_4> current_node{node->node_4};
                auto larger_node =
                    detail::inode_16::create(std::move(current_node), std::move(leaf), depth);
                *node = detail::node_ptr{larger_node.release()};

                --inode4_count;
                ++inode16_count;
                ++inode4_to_inode16_count;
                assert(inode4_to_inode16_count >= inode16_count);

            } else if (node_type == detail::node_type::I16) {
                assert(inode16_count > 0);

                std::unique_ptr<detail::inode_16> current_node{node->node_16};
                increase_memory_use(sizeof(detail::inode_48) - sizeof(detail::inode_16));
                auto larger_node =
                    detail::inode_48::create(std::move(current_node), std::move(leaf), depth);
                *node = detail::node_ptr{larger_node.release()};

                --inode16_count;
                ++inode48_count;
                ++inode16_to_inode48_count;
                assert(inode16_to_inode48_count >= inode48_count);

            } else {
                assert(inode48_count > 0);

                assert(node_type == detail::node_type::I48);
                std::unique_ptr<detail::inode_48> current_node{node->node_48};
                increase_memory_use(sizeof(detail::inode_256) - sizeof(detail::inode_48));
                auto larger_node =
                    detail::inode_256::create(std::move(current_node), std::move(leaf), depth);
                *node = detail::node_ptr{larger_node.release()};

                --inode48_count;
                ++inode256_count;
                ++inode48_to_inode256_count;
                assert(inode48_to_inode256_count >= inode256_count);
            }
            return true;
        }

        node = reinterpret_cast<detail::node_ptr*>(child);
        ++depth;
        remaining_key.shift_right(1);
    }*/
}

template <typename P>
template <typename... Args>
inline typename db<P>::iterator db<P>::emplace_key_args(std::true_type, fast_key_type key,
                                                        Args&&... args)
{
    // Emplacement support for multiset/multimap
    bitwise_key bitk(key);
    auto pos = internal_locate(bitk);
    return internal_emplace(pos, bitk, std::forward<Args>(args)...);
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
    bitwise_key bitk(key);
    auto pos = internal_locate(bitk);

    const bool should_insert = !pos.match(bitk);
    return std::make_pair(should_insert ? internal_emplace(pos, bitk, std::forward<Args>(args)...)
                                        : pos.mutable_self(),
                          should_insert);
}

template <typename P> inline typename db<P>::size_type db<P>::erase(fast_key_type key)
{
    bitwise_key bitk(key);
    auto pos = internal_locate(bitk);

    size_type removed = 0;

    if (pos.match(bitk)) {
        /*if (child_ptr->type() == node_type::LEAF) {
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
        }*/
    }

    return removed;
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
