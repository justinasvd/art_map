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
            const key_size_type prefix_length = pos.prefix_length();
            if (key.second <= prefix_length || pos.shared_prefix_length(key.first) < prefix_length)
                break;

            const auto child = inode_cast(pos.node())->find_child(key.first[prefix_length]);
            if (!child)
                break;
            pos = child;

            // Consume the explored prefix + 1 byte used during child lookup
            shift_right(key, prefix_length + 1);
        }
    }

    return pos;
}

template <typename P>
inline typename db<P>::const_iterator db<P>::internal_find(fast_key_type key) const noexcept
{
    auto bitk = make_bitwise_key_prefix(key);
    auto pos = internal_locate(bitk);
    return pos.match(key) ? pos : end();
}

template <typename P>
template <typename Node, typename... Args>
inline unique_node_ptr<Node, db<P>> db<P>::make_node_ptr(Args&&... args)
{
    using unique_ptr_t = unique_node_ptr<Node, db<P>>;

    using node_allocator_type = typename P::allocator_type::template rebind<Node>::other;
    using node_allocator_traits = std::allocator_traits<node_allocator_type>;

    node_allocator_type alloc(allocator());
    unique_ptr_t node_ptr(node_allocator_traits::allocate(alloc, 1),
                          node_deleter<Node, self_t>(*this));

    node_allocator_traits::construct(alloc, node_ptr.get(), std::forward<Args>(args)...);

    ++count<Node>();

    return node_ptr;
}

template <typename P>
template <typename... Args>
inline typename db<P>::leaf_unique_ptr db<P>::make_leaf_ptr(fast_key_type key, Args&&... args)
{
    // Allocate a single leaf
    auto leaf = make_node_ptr<leaf_type>(key);
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

    assert(count<Node>() != 0);

    node_allocator_type alloc(allocator());
    node_allocator_traits::deallocate(alloc, node, 1);

    --count<Node>();
}

template <typename P>
inline void db<P>::deallocate(leaf_type* leaf) noexcept(
    std::is_nothrow_destructible<mapped_type>::value)
{
    leaf->destroy_value(allocator());
    deallocate_node(leaf);
}

template <typename P>
inline void db<P>::deallocate(node_ptr node) noexcept(
    std::is_nothrow_destructible<mapped_type>::value)
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
        switch (parent->type()) {
        case node_type::I4:
            static_cast<inode_4*>(parent)->replace(hint, child.release());
            break;
        case node_type::I16:
            static_cast<inode_16*>(parent)->replace(hint, child.release());
            break;
        case node_type::I48:
            static_cast<inode_48*>(parent)->replace(hint, child.release());
            break;
        case node_type::I256:
            static_cast<inode_256*>(parent)->replace(hint, child.release());
            break;
        default:
            CANNOT_HAPPEN();
        }
    } else {
        if (child->type() != node_type::LEAF)
            inode_cast(child.get())->clear_parent();
        tree.root = child.release();
    }
}

template <typename P>
template <typename NodePtr>
inline typename db<P>::iterator db<P>::create_inode_4(const_iterator hint,
                                                      const bitwise_key_prefix& prefix,
                                                      NodePtr pdst, leaf_unique_ptr leaf,
                                                      key_size_type rem)
{
    auto in4 = make_node_ptr<inode_4>(prefix);
    const iterator leaf_iter = in4->add_two_to_empty(pdst, std::move(leaf), rem);
    release_to_parent(hint, std::move(in4));
    return leaf_iter;
}

template <typename P>
template <typename Source>
inline typename db<P>::iterator db<P>::grow_node(const_iterator hint, node_ptr dest_node,
                                                 leaf_unique_ptr leaf, std::uint8_t key_byte)
{
    using larger_inode = typename Source::larger_inode_type;

    assert(leaf->prefix_length() != 0);
    auto dst = static_cast<Source*>(dest_node);
    if (BOOST_LIKELY(!dst->is_full())) {
        return dst->add(std::move(leaf), key_byte);
    } else {
        // Destination node is full, needs to grow. Note that just before releasing
        // the created node to its parent, the index points to the inserted leaf.
        leaf_type* const leaf_ptr = leaf.get();
        auto larger =
            make_node_ptr<larger_inode>(make_unique_node_ptr(dst), std::move(leaf), key_byte);
        iterator leaf_iter(leaf_ptr, larger->index(), larger.get());
        release_to_parent(hint, std::move(larger));
        return leaf_iter;
    }
}

template <typename P>
template <typename... Args>
inline typename db<P>::iterator db<P>::internal_emplace(const_iterator hint,
                                                        fast_key_type original_key,
                                                        const bitwise_key_prefix& key,
                                                        Args&&... args)
{
    assert(key.first.max_size() >= key.second);

    // Preemptively create a leaf. This also ensures strong exception safety
    auto leaf_ptr = make_leaf_ptr(original_key, std::forward<Args>(args)...);

    if (BOOST_UNLIKELY(empty())) {
        assert(!hint);
        tree.root = leaf_ptr.release();
        return iterator(tree.root, 0);
    }

    assert(hint);
    assert(key.second != 0);

    const node_type dst_type = hint.type();

    if (dst_type == node_type::LEAF) {
        leaf_type* const pdst = static_cast<leaf_type*>(hint.node());

        // Can only happen in multivalued container case
        if (BOOST_UNLIKELY(pdst->key() == leaf_ptr->key())) {
            pdst->push_back(std::move(leaf_ptr->value()));
            return iterator(hint);
        }

        // Put the 2 leaves under the single inode_4
        return create_inode_4(hint, pdst->shared_prefix(key.first, key.second - 1), pdst,
                              std::move(leaf_ptr), key.first.max_size() - key.second);
    }

    // Some other node, not a leaf
    const node_ptr pdst = hint.node();
    const key_size_type prefix_len = pdst->prefix_length();

    // If the prefix is not fully shared, split the prefix
    {
        const auto shared_prefix = pdst->shared_prefix(key.first);
        if (shared_prefix.second < prefix_len) {
            return create_inode_4(hint, shared_prefix, pdst, std::move(leaf_ptr),
                                  key.first[shared_prefix.second]);
        }
    }

    assert(key.second > prefix_len);

    // Key byte for this leaf. Pass it on to the nodes
    const std::uint8_t key_byte = key.first[prefix_len];

    // Add the newly created leaf to the proper node.
    // If that node is full, then a new larger node will be created,
    // and the leaf will be added there.
    switch (dst_type) {
    case node_type::I4:
        return grow_node<inode_4>(hint, pdst, std::move(leaf_ptr), key_byte);
    case node_type::I16:
        return grow_node<inode_16>(hint, pdst, std::move(leaf_ptr), key_byte);
    case node_type::I48:
        return grow_node<inode_48>(hint, pdst, std::move(leaf_ptr), key_byte);
    default:
        assert(dst_type == node_type::I256);
        return static_cast<inode_256*>(pdst)->add(std::move(leaf_ptr), key_byte);
    }
}

template <typename P>
template <typename... Args>
inline typename db<P>::iterator db<P>::emplace_key_args(std::true_type, fast_key_type key,
                                                        Args&&... args)
{
    // Emplacement support for multiset/multimap
    auto bitk = make_bitwise_key_prefix(key);
    auto pos = internal_locate(bitk);
    return internal_emplace(pos, key, bitk, std::forward<Args>(args)...);
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

    const bool should_insert = !pos.match(key);
    return std::make_pair(should_insert
                              ? internal_emplace(pos, key, bitk, std::forward<Args>(args)...)
                              : iterator(pos),
                          should_insert);
}

template <typename P> template <typename Source> inline void db<P>::shrink_node(const_iterator pos)
{
    auto src = static_cast<Source*>(pos.parent());
    if (!src->is_min_size()) {
        src->remove(pos.index());
    } else {
        // If allocation fails here, the original node will be left untouched,
        // which gives the shrinking operation the strong exception safety guarantee
        auto dst = make_smaller_node(*src, pos.index());
        release_to_parent(src->self_iterator(), std::move(dst));
        // All went well, we can deallocate the original node
        deallocate(src);
    }
}

template <typename P> inline void db<P>::internal_erase(const_iterator pos)
{
    assert(pos && pos.type() == node_type::LEAF);

    // Also remove a leaf from its parent. In case of the no parent case,
    // we'll simply delete the root leaf
    const auto leaf_parent = pos.parent();

    if (BOOST_UNLIKELY(leaf_parent == nullptr)) {
        assert(pos.node() == tree.root);
        tree.root = nullptr;
    } else {
        // Remove the value from to the parent node.
        // If that node is already of minimum size, then a new smaller
        // node will be created without the value to remove.
        switch (leaf_parent->type()) {
        case node_type::I4:
            shrink_node<inode_4>(pos);
            break;
        case node_type::I16:
            shrink_node<inode_16>(pos);
            break;
        case node_type::I48:
            shrink_node<inode_48>(pos);
            break;
        default:
            assert(leaf_parent->type() == node_type::I256);
            shrink_node<inode_256>(pos);
        }
    }

    // All went well, we can deallocate the leaf
    deallocate(static_cast<leaf_type*>(pos.node()));
}

template <typename P> inline typename db<P>::size_type db<P>::erase(fast_key_type key)
{
    auto bitk = make_bitwise_key_prefix(key);
    auto pos = internal_locate(bitk);

    size_type removed = 0;

    if (pos.match(key)) {
        removed = static_cast<leaf_type*>(pos.node())->size();
        internal_erase(pos);
    }

    return removed;
}

template <typename P> inline typename db<P>::iterator db<P>::erase(iterator pos)
{
    if (pos != end()) {
        const iterator next = std::next(pos);
        internal_erase(pos);
        pos = next;
    }
    return pos;
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
    os << "leaf size = " << sizeof(leaf_type) << ", current memory use = " << current_memory_use()
       << '\n';
    inode::dump(os, tree.root);
}

} // namespace detail
} // namespace art

#endif // ART_DETAIL_ART_CONTAINER_IMPL_HEADER_INCLUDED
