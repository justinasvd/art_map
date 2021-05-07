#ifndef ART_DETAIL_ART_CONTAINER_IMPL_HEADER_INCLUDED
#define ART_DETAIL_ART_CONTAINER_IMPL_HEADER_INCLUDED

#include "art_container.h"
#include "art_deleter.h"
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
    const_iterator pos(tree.root, 0);

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
template <typename Node, typename... Args>
inline std::unique_ptr<Node, node_deleter<Node, db<P>>> db<P>::make_node_ptr(Args&&... args)
{
    using unique_node_ptr = std::unique_ptr<Node, node_deleter<Node, db<P>>>;

    using node_allocator_type = typename P::allocator_type::template rebind<Node>::other;
    using node_allocator_traits = std::allocator_traits<node_allocator_type>;

    node_allocator_type alloc(allocator());
    unique_node_ptr node_ptr(node_allocator_traits::allocate(alloc, 1),
                             node_deleter<Node, self_t>(*this));

    node_allocator_traits::construct(alloc, node_ptr.get(), std::forward<Args>(args)...);

    increase_memory_use<Node>();
    return node_ptr;
}

template <typename P>
template <typename... Args>
inline typename db<P>::leaf_unique_ptr db<P>::make_leaf_ptr(const bitwise_key_prefix& key,
                                                            Args&&... args)
{
    // Allocate a single leaf
    auto leaf = make_node_ptr<leaf_type>(key.first, key.second);
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

    decrease_memory_use<Node>();
}

template <typename P> inline void db<P>::deallocate(leaf_type* leaf) noexcept
{
    leaf->destroy_value(allocator());
    deallocate_node(leaf);
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
inline void db<P>::release_to_parent(const_iterator hint, NodePtr child) noexcept
{
    if (BOOST_LIKELY(hint.parent() != nullptr)) {
        const auto parent = hint.parent();
        assert(parent->type() != node_type::LEAF);
        // Downcast the child pointer. It doesn't really matter here that the
        // "generic" deleter is slow, because ideally we won't have to call that deleter.
        parent->replace(hint.position, make_unique_node_ptr<node_base>(child.release()));
    } else {
        tree.root = child.release();
    }
}

template <typename P>
inline void db<P>::create_inode_4(const_iterator hint, const bitwise_key_prefix& prefix,
                                  node_ptr pdst, leaf_unique_ptr leaf)
{
    auto new_node = make_node_ptr<inode_4>(prefix);
    new_node->add_two_to_empty(pdst, std::move(leaf));
    release_to_parent(hint, std::move(new_node));
}

template <typename P>
template <typename Source, typename Dest>
inline void db<P>::grow_node(const_iterator hint, node_ptr source_node, leaf_unique_ptr leaf)
{
    assert(leaf->prefix_length() != 0);
    auto src = make_unique_node_ptr(static_cast<Source*>(source_node));
    auto dst = make_node_ptr<Dest>(std::move(src), std::move(leaf));
    release_to_parent(hint, std::move(dst));
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
        tree.root = leaf_ptr.release();
        return leaf_iter;
    }

    assert(hint);
    const node_ptr pdst = hint.node;
    const node_type dst_type = pdst->type();

    if (dst_type == node_type::LEAF) {
        // Can only happen in multivalued container case
        if (BOOST_UNLIKELY(pdst->match(key.first))) {
            // Not yet supported
            assert(false);
        }

        const key_size_type min_ksize = std::min(pdst->prefix_length(), key.second);
        assert(min_ksize != 0);

        // Put the 2 leaves under the single inode_4
        create_inode_4(hint, pdst->shared_prefix(key.first, min_ksize - 1), pdst,
                       std::move(leaf_ptr));
        return leaf_iter;
    }

    if (key.second > pdst->prefix_length()) {
        // Directly compute shared prefix. This eliminates computing shared
        // prefix length twice.
        const auto shared_prefix = pdst->shared_prefix(key.first);
        if (shared_prefix.second < pdst->prefix_length()) {
            // Needs to split the key prefix
            create_inode_4(hint, shared_prefix, pdst, std::move(leaf_ptr));
            return leaf_iter;
        }
    }

    if (!inode_cast(pdst)->add(leaf_ptr)) {
        // The destination node is full. Resize the destination node
        if (dst_type == node_type::I4) {
            grow_node<inode_4, inode_16>(hint, pdst, std::move(leaf_ptr));
        } else if (dst_type == node_type::I16) {
            grow_node<inode_16, inode_48>(hint, pdst, std::move(leaf_ptr));
        } else {
            assert(dst_type == node_type::I48);
            grow_node<inode_48, inode_256>(hint, pdst, std::move(leaf_ptr));
        }
    }

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

template <typename P>
template <typename Source, typename Dest>
inline void db<P>::shrink_node(const_iterator pos) noexcept
{
    auto src = make_unique_node_ptr(static_cast<Source*>(pos.parent()));
    auto rem = make_node_ptr<Dest>(std::move(src), pos.position, *this);
    release_to_parent(pos.parent()->iterator(), std::move(rem));
}

template <typename P> inline typename db<P>::size_type db<P>::erase(fast_key_type key)
{
    auto bitk = make_bitwise_key_prefix(key);
    auto pos = internal_locate(bitk);

    size_type removed = 0;

    if (pos.match(bitk.first)) {
        assert(pos);

        // Remove a leaf from its parent. In case of the no parent case,
        // we'll simply delete the root leaf
        const auto parent = pos.parent();

        if (BOOST_UNLIKELY(parent == nullptr)) {
            assert(pos.node == tree.root);
            deallocate(static_cast<leaf_type*>(tree.root));
            tree.root = nullptr;
        } else if (!parent->remove(pos.position, *this)) {
            const node_type parent_type = parent->type();

            // We have failed to remove the value from the node because
            // the node would become undersized after the operation. We have
            // to shrink the node and remove the value at the same token.
            if (parent_type == node_type::I4) {
                auto src = make_unique_node_ptr(static_cast<inode_4*>(parent));
                auto rem = src->leave_last_child(pos.position, *this);
                release_to_parent(parent->iterator(), std::move(rem));
            } else if (parent_type == node_type::I16) {
                shrink_node<inode_16, inode_4>(pos);
            } else if (parent_type == node_type::I48) {
                shrink_node<inode_48, inode_16>(pos);
            } else {
                assert(parent_type == node_type::I256);
                shrink_node<inode_256, inode_48>(pos);
            }
        }

        removed = 1;
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
    std::swap(tree.root, other.tree.root);

    // Swap the stats
    std::swap(current_memory_use_, other.current_memory_use_);
    std::swap(count_, other.count_);
}

template <typename P> inline void db<P>::clear()
{
    if (!empty()) {
        delete_subtree(tree.root);
        assert(leaf_count() == 0);
        tree.root = nullptr;
    }
}

template <typename P> inline void db<P>::dump(std::ostream& os) const
{
    os << "node size = " << sizeof(node_base) << ", current memory use = " << current_memory_use()
       << '\n';
    inode::dump(os, tree.root);
}

} // namespace detail
} // namespace art

#endif // ART_DETAIL_ART_CONTAINER_IMPL_HEADER_INCLUDED
