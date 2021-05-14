// Copyright 2021 Justinas V. Daugmaudis
#ifndef ART_DETAIL_ART_ITERATOR_HEADER_INCLUDED
#define ART_DETAIL_ART_ITERATOR_HEADER_INCLUDED

#include <type_traits>

#include <boost/config.hpp> // likely/unlikely macros

namespace art
{
namespace detail
{

// Forward declaration of the container
template <typename Traits> class db;

template <typename Traits, typename Node, typename INode> struct tree_iterator {
    using key_type = typename Traits::key_type;
    using fast_key_type = typename Traits::fast_key_type;

    using value_type = typename Traits::value_type;
    using size_type = typename Traits::size_type;
    using difference_type = typename Traits::difference_type;
    using const_pointer = typename Traits::const_pointer;
    using const_reference = typename Traits::const_reference;

    static constexpr bool is_const = std::is_const<Traits>::value;

    using pointer = std::conditional_t<is_const, const_pointer, typename Traits::pointer>;
    using reference = std::conditional_t<is_const, const_reference, typename Traits::reference>;

    using iterator_category = std::bidirectional_iterator_tag;

    constexpr tree_iterator() noexcept
        : node_(nullptr)
        , parent_(nullptr)
        , pos_in_parent(0)
    {
    }

    // Default copy c-tor is fine
    tree_iterator(const tree_iterator& rhs) = default;

    constexpr tree_iterator(Node* node, unsigned int index, INode* parent = nullptr) noexcept
        : node_(node)
        , parent_(parent)
        , pos_in_parent(index)
    {
    }

    // Accessors for the key/value the iterator is pointing at.
    // fast_key_type key() const noexcept { return node->key(position); }
    // pointer operator->() const noexcept { return &node->ref_value(position); }
    // reference operator*() const noexcept { return node->ref_value(position); }

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
        ++*this;
        return tmp;
    }
    tree_iterator operator--(int) noexcept
    {
        tree_iterator tmp = *this;
        --*this;
        return tmp;
    }

    explicit operator bool() const noexcept { return node_ != nullptr; }

    // Enable assignments between const and non-const iterators
    template <typename OtherTraits,
              typename = typename std::is_same<std::remove_cv_t<Traits>,
                                               std::remove_cv_t<OtherTraits>>::type>
    tree_iterator& operator=(const tree_iterator<OtherTraits, Node, INode>& rhs) noexcept
    {
        node_ = rhs.node();
        parent_ = rhs.parent();
        pos_in_parent = rhs.index();
        return *this;
    }

    [[nodiscard]] Node* node() const noexcept { return node_; }
    [[nodiscard]] INode* parent() const noexcept { return parent_; }
    [[nodiscard]] unsigned int index() const noexcept { return pos_in_parent; }

private:
    using traits_type = std::remove_const_t<Traits>;

    using bitwise_key = typename Traits::bitwise_key;
    using key_size_type = typename bitwise_key::size_type;

    using mutable_tree_iterator = tree_iterator<traits_type, Node, INode>;

    friend INode;
    friend db<traits_type>;

    [[nodiscard]] mutable_tree_iterator mutable_self() const noexcept
    {
        return mutable_tree_iterator(node_, pos_in_parent, parent_);
    }

    [[nodiscard]] node_type type() const noexcept { return node_->type(); }

    [[nodiscard]] static bool is_leaf(const Node* node) noexcept
    {
        return node && (node->type() == node_type::LEAF);
    }
    [[nodiscard]] bool is_leaf() const noexcept { return is_leaf(node_); }
    [[nodiscard]] bool match(bitwise_key key) const noexcept
    {
        return is_leaf() && node_->match(key);
    }
    [[nodiscard]] key_size_type prefix_length() const noexcept { return node_->prefix_length(); }
    [[nodiscard]] key_size_type shared_prefix_length(bitwise_key key) const noexcept
    {
        return node_->shared_prefix_length(key);
    }

    void increment() noexcept
    {
        if (BOOST_UNLIKELY(!is_leaf())) {
            // Nothing can do, not a leaf
            return;
        }

        if (BOOST_UNLIKELY(!parent_)) {
            // Root node, just set the position to past-end state
            pos_in_parent = 1;
            return;
        }

        do {
            ++pos_in_parent;
            if (pos_in_parent < INode::capacity(parent_)) {
                // Parent node is not yet exhausted, try to find a leftmost leaf
                // from the current position.
                auto leaf = INode::leftmost_leaf(parent_, pos_in_parent);
                if (leaf) {
                    *this = leaf;
                    return;
                }
            }

            // Parent node has been exhausted. Try going one level up
            *this = parent_->self_iterator();
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
            pos_in_parent = 0;
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
    // The node in the tree the iterator is pointing at.
    Node* node_;
    // Parent of the current node. Also, the position below is within this parent.
    INode* parent_;
    // The position within the parent node of the node.
    unsigned int pos_in_parent;
};

// Enable comparisons between differently cv-qualified nodes
template <
    typename Traits1, typename Traits2, typename Node, typename INode,
    typename = typename std::is_same<std::remove_cv_t<Traits1>, std::remove_cv_t<Traits2>>::type>
inline bool operator==(const tree_iterator<Traits1, Node, INode>& lhs,
                       const tree_iterator<Traits2, Node, INode>& rhs) noexcept
{
    return lhs.node() == rhs.node() && lhs.parent() == rhs.parent() && lhs.index() == rhs.index();
}

template <typename Traits1, typename Traits2, typename Node, typename INode>
inline bool operator!=(const tree_iterator<Traits1, Node, INode>& lhs,
                       const tree_iterator<Traits2, Node, INode>& rhs) noexcept
{
    return !(lhs == rhs);
}

} // namespace detail
} // namespace art

#endif // ART_DETAIL_ART_ITERATOR_HEADER_INCLUDED
