// Copyright 2021 Justinas V. Daugmaudis
#ifndef ART_DETAIL_ART_ITERATOR_HEADER_INCLUDED
#define ART_DETAIL_ART_ITERATOR_HEADER_INCLUDED

#include "art_node_base.h"
#include "node_type.h"

#include <type_traits>

#include <boost/config.hpp> // likely/unlikely macros

namespace art
{
namespace detail
{

// Forward declaration of the container
template <typename Traits> class db;

template <typename Traits, typename INode> class tree_iterator
{
    using bitwise_key = typename Traits::bitwise_key;

    using fast_key_type = typename Traits::fast_key_type;
    using real_leaf_type = typename Traits::leaf_type;

    using node_ptr = typename Traits::node_ptr;

    static constexpr bool is_const = std::is_const<INode>::value;

    // Const-corrected leaf type
    using leaf_type = std::conditional_t<is_const, const real_leaf_type, real_leaf_type>;

    // This helper here is to give a pointer to the operator->().
    // Note that this proxy will never hold a leaf value (so no actual
    // value copying is involved), but rather the (const) reference to
    // that value.
    template <typename T> struct arrow_proxy {
        T vref;
        constexpr T* operator->() noexcept { return std::addressof(vref); }
    };

    using inode_type = std::remove_const_t<INode>;

    // Const and non-const versions of iterators are friends
    friend tree_iterator<Traits, const INode>;
    friend tree_iterator<Traits, inode_type>;

public:
    using key_type = typename Traits::key_type;
    using value_type = typename Traits::value_type;
    using size_type = typename Traits::size_type;
    using const_reference = typename Traits::const_reference;
    using const_pointer = arrow_proxy<const_reference>;
    using reference = std::conditional_t<is_const, const_reference, typename Traits::reference>;
    using pointer = arrow_proxy<reference>;
    using difference_type = typename Traits::difference_type;
    using iterator_category = std::bidirectional_iterator_tag;

    // Uninitialized iterator
    constexpr tree_iterator() noexcept = default;

    // Default copy c-tor is fine
    tree_iterator(const tree_iterator& rhs) noexcept = default;

    // Enable copy construction between const and non-const iterators
    template <typename U>
    explicit constexpr tree_iterator(const tree_iterator<Traits, U>& rhs) noexcept
        : node_(rhs.node_)
        , parent_(rhs.parent_)
        , position(rhs.position)
    {
    }

    explicit constexpr tree_iterator(node_ptr node, int index = 0,
                                     node_ptr parent = node_ptr{}) noexcept
        : node_(node)
        , parent_(parent)
        , position(index)
    {
    }

    // Accessors for the key/value the iterator is pointing at
    pointer operator->() const noexcept { return arrow_proxy<reference>{iter_deref()}; }
    reference operator*() const noexcept { return iter_deref(); }

    // Increment/decrement the iterator.
    tree_iterator& operator++() noexcept
    {
        increment();
        return *this;
    }
    tree_iterator& operator--() noexcept
    {
        decrement();
        return *this;
    }
    tree_iterator operator++(int) noexcept
    {
        tree_iterator tmp = *this;
        increment();
        return tmp;
    }
    tree_iterator operator--(int) noexcept
    {
        tree_iterator tmp = *this;
        decrement();
        return tmp;
    }

    // Enable assignments between const and non-const iterators
    template <typename U> tree_iterator& operator=(const tree_iterator<Traits, U>& rhs) noexcept
    {
        node_ = rhs.node_;
        parent_ = rhs.parent_;
        position = rhs.position;
        return *this;
    }

    [[nodiscard]] node_ptr node() const noexcept { return node_; }
    [[nodiscard]] node_ptr parent() const noexcept { return parent_; }
    [[nodiscard]] int index() const noexcept { return position; }

    void dump(std::ostream& os) const { INode::dump(os, parent_); }

private:
    friend db<Traits>;
    friend INode;

    [[nodiscard]] node_type tag() const noexcept { return node_.tag(); }
    [[nodiscard]] static bool is_leaf(const node_ptr node) noexcept
    {
        return node && node.tag() == node_type::LEAF;
    }
    [[nodiscard]] bool is_leaf() const noexcept { return is_leaf(node_); }
    [[nodiscard]] bool match(fast_key_type key) const noexcept
    {
        return is_leaf() && node_->prefix() == bitwise_key(key);
    }

    [[nodiscard]] typename Traits::node_base* node_base() const noexcept { return node_.get(); }
    [[nodiscard]] real_leaf_type* leaf() const noexcept
    {
        assert(is_leaf());
        return static_cast<real_leaf_type*>(node_base());
    }

    [[nodiscard]] reference iter_deref() const noexcept
    {
        leaf_type* const l = leaf();
        return Traits::value_ref(l->prefix().unpack(), l->value());
    }

    void increment() noexcept
    {
        if (BOOST_UNLIKELY(!is_leaf())) {
            // Nothing can do, not a leaf
            return;
        }

        if (BOOST_UNLIKELY(!parent_)) {
            // Root node, just set the position to past-end state.
            // It's not demanded by the standard, but we implement this
            // in such a way that end() == std::next(end())
            position = std::min(position + 1, 1);
            return;
        }

        assert(parent_.tag() != node_type::LEAF);

        do {
            // Parent node is not yet exhausted, try to find a leftmost leaf
            // from the current position.
            if (position < 255) {
                auto leaf = INode::leftmost_leaf(parent_, position + 1);
                if (leaf.node()) {
                    assert(leaf.parent() != nullptr);
                    *this = leaf;
                    return;
                }
            }

            // Parent node has been exhausted. Try going one level up
            *this = static_cast<inode_type*>(parent_.get())->self_iterator(parent_.tag());
        } while (parent_ != nullptr);
    }

    void decrement() noexcept
    {
        if (BOOST_UNLIKELY(!is_leaf())) {
            // Nothing can do, not a leaf
            return;
        }

        if (BOOST_UNLIKELY(!parent_)) {
            // Root node, just set the position to start state
            position = 0;
            return;
        }

        // Non leaves cannot be decremented
        //     assert(position <= -1);
        //     btree_iterator save(*this);
        //     while (position < 0 && !node->is_root()) {
        //         assert(node->parent()->child(node->position()) == node);
        //         position = node->position() - 1;
        //         node = node->parent();
        //     }
        //     if (position < 0) {
        //         *this = save;
        //     }
    }

private:
    // node_: The node in the tree the iterator is pointing at
    // parent_: Parent of the current node
    node_ptr node_, parent_;
    // The position within the parent node of the node.
    int position;
};

// Enable comparisons between differently cv-qualified nodes
template <
    typename Traits, typename INode1, typename INode2,
    typename = typename std::is_same<std::remove_cv_t<INode1>, std::remove_cv_t<INode2>>::type>
inline bool operator==(const tree_iterator<Traits, INode1>& lhs,
                       const tree_iterator<Traits, INode2>& rhs) noexcept
{
    // It is not necessary to check that parents are the same -- node pointers
    // must always be unique. However, we need to ensure that we don't inadvertedly
    // report true for begin() == end(), when we compare root leaves
    return lhs.node() == rhs.node() && lhs.index() == rhs.index();
}

template <typename Traits, typename INode1, typename INode2>
inline bool operator!=(const tree_iterator<Traits, INode1>& lhs,
                       const tree_iterator<Traits, INode2>& rhs) noexcept
{
    return !(lhs == rhs);
}

} // namespace detail
} // namespace art

#endif // ART_DETAIL_ART_ITERATOR_HEADER_INCLUDED
