// Copyright 2021 Justinas V. Daugmaudis
#ifndef ART_DETAIL_ART_ITERATOR_HEADER_INCLUDED
#define ART_DETAIL_ART_ITERATOR_HEADER_INCLUDED

#include <type_traits>

namespace art
{
namespace detail
{
template <typename Traits, typename NodePtr> struct tree_iterator {
    using key_type = typename Traits::key_type;
    using fast_key_type = typename Traits::fast_key_type;

    using value_type = typename Traits::value_type;
    using size_type = typename Traits::size_type;
    using difference_type = typename Traits::difference_type;
    using const_pointer = typename Traits::const_pointer;
    using const_reference = typename Traits::const_reference;

    static constexpr bool is_const = std::is_const<NodePtr>::value;

    using pointer = std::conditional_t<is_const, const_pointer, typename Traits::pointer>;
    using reference = std::conditional_t<is_const, const_reference, typename Traits::reference>;

    using iterator_category = std::bidirectional_iterator_tag;

    constexpr tree_iterator() noexcept
        : node(nullptr)
    //, position(-1)
    {
    }

    constexpr tree_iterator(NodePtr n, int p) noexcept
        : node(n)
    //, position(p)
    {
    }

    // Default copy c-tor is fine
    tree_iterator(const tree_iterator& rhs) = default;

    // // Const-converting c-tor
    // template <typename Other>
    // tree_iterator(tree_iterator<Other> rhs)
    //     : node(const_cast<Node*>(rhs.node))
    //     , position(rhs.position)
    // {
    // }

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

    bool operator==(std::nullptr_t) noexcept { return node == nullptr; }
    bool operator!=(std::nullptr_t) noexcept { return node != nullptr; }

private:
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
    NodePtr node;
    // The position within the node of the tree the iterator is pointing at.
    // int position;
};

// Enable comparisons between differently cv-qualified nodes
template <typename Traits, typename U, typename V,
          typename = typename std::is_same<std::remove_cv_t<U>, std::remove_cv_t<V>>::type>
inline bool operator==(const tree_iterator<Traits, U>& lhs,
                       const tree_iterator<Traits, V>& rhs) noexcept
{
    return lhs.node == rhs.node && lhs.position == rhs.position;
}
template <typename Traits, typename U, typename V>
inline bool operator!=(const tree_iterator<Traits, U>& lhs,
                       const tree_iterator<Traits, V>& rhs) noexcept
{
    return !(lhs == rhs);
}

} // namespace detail
} // namespace art

#endif // ART_DETAIL_ART_ITERATOR_HEADER_INCLUDED
