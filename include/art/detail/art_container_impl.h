#ifndef ART_DETAIL_ART_CONTAINER_IMPL_HEADER_INCLUDED
#define ART_DETAIL_ART_CONTAINER_IMPL_HEADER_INCLUDED

#include "art_container.h"
#include "art_nodes.h"

#include <boost/config.hpp>

namespace art
{
namespace detail
{

template <typename BitwiseKey>
inline constexpr void shift_right(std::pair<BitwiseKey, typename BitwiseKey::size_type>& lhs,
                                  typename BitwiseKey::size_type size) noexcept
{
    assert(lhs.second >= size);
    lhs.first.shift_right(size);
    lhs.second -= size;
}

template <typename P>
inline typename db<P>::const_iterator db<P>::internal_locate(bitwise_key_prefix& key) const noexcept
{
    const_iterator pos(data.root, 0);

    if (pos) {
        while (pos.type() != node_type::LEAF) {
            const unsigned prefix_length = pos.prefix_length();
            if (key.second <= prefix_length || pos.shared_prefix_length(key.first) < prefix_length)
                break;
            shift_right(key, prefix_length);

            const auto child = inode_cast(pos.node)->find_child(key.first.front());
            if (!child)
                break;
            pos = child;

            // We have consumed one byte during the child lookup
            shift_right(key, 1);
        }
    }

    return pos;
}

template <typename P>
inline typename db<P>::const_iterator db<P>::internal_find(fast_key_type key) const noexcept
{
    auto bitk = make_bitwise_key_prefix(key);
    auto pos = internal_locate(bitk);
    return pos.match(bitk.first) ? pos : end();
}

template <typename P>
template <typename Node>
inline std::unique_ptr<Node, node_deleter<Node, db<P>>> db<P>::make_node_ptr(
    const bitwise_key_prefix& key, node_ptr parent)
{
    using unique_node_ptr = std::unique_ptr<Node, node_deleter<Node, db<P>>>;

    using node_allocator_type = typename P::allocator_type::template rebind<Node>::other;
    using node_allocator_traits = std::allocator_traits<node_allocator_type>;

    assert(!parent || parent->type() != node_type::LEAF);

    node_allocator_type alloc(allocator());
    unique_node_ptr node_ptr(node_allocator_traits::allocate(alloc, 1),
                             node_deleter<Node, self_t>(*this));

    node_allocator_traits::construct(alloc, node_ptr.get(), key.first, key.second, parent);

    increase_memory_use(sizeof(Node));
    return node_ptr;
}

template <typename P>
inline typename db<P>::leaf_unique_ptr db<P>::make_leaf_ptr(const bitwise_key_prefix& key)
{
    // Allocate a single leaf
    leaf_unique_ptr leaf_ptr = make_node_ptr<leaf_type>(key);
    ++leaf_count_;
    return leaf_ptr;
}

template <typename P>
template <typename... Args>
inline typename db<P>::leaf_unique_ptr db<P>::make_leaf_ptr(const bitwise_key_prefix& key,
                                                            Args&&... args)
{
    auto leaf = make_leaf_ptr(key);
    // Emplace the value into the leaf
    leaf->emplace_value(allocator(), std::forward<Args>(args)...);
    return leaf;
}

template <typename P>
template <typename Node>
inline void db<P>::deallocate_node(Node* node) noexcept
{
    using node_allocator_type = typename P::allocator_type::template rebind<Node>::other;
    using node_allocator_traits = std::allocator_traits<node_allocator_type>;

    node_allocator_type alloc(allocator());
    node_allocator_traits::deallocate(alloc, node, 1);

    decrease_memory_use(sizeof(Node));
}

template <typename P> inline void db<P>::deallocate(inode_4* node) noexcept
{
    deallocate_node<inode_4>(node);
    assert(inode4_count_ != 0);
    --inode4_count_;
}

template <typename P> inline void db<P>::deallocate(inode_16* node) noexcept
{
    deallocate_node<inode_16>(node);
    assert(inode16_count_ != 0);
    --inode16_count_;
}

template <typename P> inline void db<P>::deallocate(inode_48* node) noexcept
{
    deallocate_node<inode_48>(node);
    assert(inode48_count_ != 0);
    --inode48_count_;
}

template <typename P> inline void db<P>::deallocate(inode_256* node) noexcept
{
    deallocate_node<basic_inode_256<self_t>>(node);
    assert(inode256_count_ != 0);
    --inode256_count_;
}

template <typename P> inline void db<P>::deallocate(leaf_type* leaf) noexcept
{
    leaf->destroy_value(allocator());
    deallocate_node<leaf_type>(leaf);
    assert(leaf_count_ != 0);
    --leaf_count_;
}

template <typename P> inline void db<P>::deallocate(node_ptr node) noexcept
{
    switch (node->type()) {
    case node_type::I4:
        deallocate(static_cast<inode_4*>(node));
        break;
    case node_type::I16:
        deallocate(static_cast<inode_16*>(node));
        break;
    case node_type::I48:
        deallocate(static_cast<inode_48*>(node));
        break;
    case node_type::I256:
        deallocate(static_cast<inode_256*>(node));
        break;
    case node_type::LEAF:
        deallocate(static_cast<leaf_type*>(node));
        break;
    default:
        CANNOT_HAPPEN();
    }
}

template <typename P>
template <typename NodePtr>
inline void db<P>::release_to_parent(const_iterator hint, NodePtr&& child) noexcept
{
    if (child->parent()) {
        // Release to the parent
        assert(child->parent() == hint.node);
        assert(false);
    } else {
        data.root = child.release();
    }
}

template <typename P>
template <typename... Args>
inline typename db<P>::iterator db<P>::internal_emplace(const_iterator hint,
                                                        const bitwise_key_prefix& key,
                                                        Args&&... args)
{
    assert(key.first.max_size() >= key.second);

    // Preemptively create a leaf. This also ensures strong exception safety
    auto leaf_ptr = make_leaf_ptr(key, std::forward<Args>(args)...);

    // We always return the iterator pointing to the new leaf
    const iterator leaf_iter(leaf_ptr.get(), 0);

    if (BOOST_UNLIKELY(empty())) {
        assert(!hint);
        data.root = leaf_ptr.release();
    } else {
        node_ptr pdst = hint ? hint.node : data.root;

        const key_size_type min_ksize = std::min(pdst->prefix_length(), key.second);

        if (pdst->type() == node_type::LEAF) {
            // Can only happen in multivalued container case
            if (BOOST_UNLIKELY(pdst->match(key.first))) {
                // Not yet supported
                assert(false);
            }

            assert(min_ksize > 1);

            // Put the 2 leaves under the single inode_4 and set the
            // new inode_4's parent to the existing nodes's parent.
            auto new_node = make_node_ptr<inode_4>(pdst->shared_prefix(key.first, min_ksize - 1),
                                                   pdst->parent());
            ++inode4_count_;

            new_node->add_two_to_empty(pdst, std::move(leaf_ptr));
            release_to_parent(hint, std::move(new_node));
        } else {

            assert(false);
        }
    }

    /*
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

    return leaf_iter;
}

template <typename P>
template <typename... Args>
inline typename db<P>::iterator db<P>::emplace_key_args(std::true_type, fast_key_type key,
                                                        Args&&... args)
{
    // Emplacement support for multiset/multimap
    auto bitk = make_bitwise_key_prefix(key);
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
    auto bitk = make_bitwise_key_prefix(key);
    auto pos = internal_locate(bitk);

    const bool should_insert = !pos.match(bitk.first);
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
}

template <typename P> inline void db<P>::clear()
{
    if (!empty()) {
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

template <typename P> inline void db<P>::dump(std::ostream& os) const
{
    os << "node size = " << sizeof(node_base) << ", current memory use = " << current_memory_use()
       << '\n';
    inode::dump(os, data.root);
}

} // namespace detail
} // namespace art

#endif // ART_DETAIL_ART_CONTAINER_IMPL_HEADER_INCLUDED
