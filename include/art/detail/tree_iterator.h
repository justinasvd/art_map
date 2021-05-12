// Copyright 2021 Justinas V. Daugmaudis
#ifndef ART_DETAIL_ART_ITERATOR_HEADER_INCLUDED
#define ART_DETAIL_ART_ITERATOR_HEADER_INCLUDED

#include <type_traits>

namespace art
{
namespace detail
{

// Forward declaration of the container
template <typename Traits> class db;

template <typename Traits, typename NodePtr, typename ParentPtr> struct tree_iterator {
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

    constexpr tree_iterator(NodePtr node, unsigned int index, ParentPtr parent = nullptr) noexcept
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

    [[nodiscard]] NodePtr node() const noexcept { return node_; }
    [[nodiscard]] ParentPtr parent() const noexcept { return parent_; }
    [[nodiscard]] unsigned int index() const noexcept { return pos_in_parent; }

private:
    using traits_type = std::remove_const_t<Traits>;
    friend class db<traits_type>;

    using bitwise_key = typename Traits::bitwise_key;
    using key_size_type = typename bitwise_key::size_type;

    using mutable_tree_iterator = tree_iterator<traits_type, NodePtr, ParentPtr>;
    using const_tree_iterator = tree_iterator<const traits_type, NodePtr, ParentPtr>;

    [[nodiscard]] mutable_tree_iterator mutable_self() const noexcept
    {
        return mutable_tree_iterator(node_, pos_in_parent, parent_);
    }

    explicit operator bool() const noexcept { return node_ != nullptr; }

    [[nodiscard]] node_type type() const noexcept { return node_->type(); }

    [[nodiscard]] bool match(bitwise_key key) const noexcept
    {
        return node_ && (node_->type() == node_type::LEAF) && node_->match(key);
    }
    [[nodiscard]] key_size_type prefix_length() const noexcept { return node_->prefix_length(); }
    [[nodiscard]] key_size_type shared_prefix_length(bitwise_key key) const noexcept
    {
        return node_->shared_prefix_length(key);
    }

    void increment() noexcept
    {
        /*if (node->is_leaf() && ++position < node->count()) {
            return;
        }
        increment_slow();*/
    }

    void decrement() noexcept
    {
        /*if (node->is_leaf() && --position >= 0) {
            return;
        }
        decrement_slow();*/
    }

private:
    // The node in the tree the iterator is pointing at.
    NodePtr node_;
    // Parent of the current node. Also, the position below is within this parent.
    ParentPtr parent_;
    // The position within the parent node of the node.
    unsigned int pos_in_parent;
};

// Enable comparisons between differently cv-qualified nodes
template <
    typename Traits, typename OtherTraits, typename NodePtr, typename ParentPtr,
    typename = typename std::is_same<std::remove_cv_t<Traits>, std::remove_cv_t<OtherTraits>>::type>
inline bool operator==(const tree_iterator<Traits, NodePtr, ParentPtr>& lhs,
                       const tree_iterator<OtherTraits, NodePtr, ParentPtr>& rhs) noexcept
{
    return lhs.node() == rhs.node() && lhs.parent() == rhs.parent() && lhs.index() == rhs.index();
}

template <typename Traits1, typename Traits2, typename Node, typename Parent>
inline bool operator!=(const tree_iterator<Traits1, Node, Parent>& lhs,
                       const tree_iterator<Traits2, Node, Parent>& rhs) noexcept
{
    return !(lhs == rhs);
}

} // namespace detail
} // namespace art

#endif // ART_DETAIL_ART_ITERATOR_HEADER_INCLUDED
