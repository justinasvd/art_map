#ifndef ART_DETAIL_ART_CONTAINER_HEADER_INCLUDED
#define ART_DETAIL_ART_CONTAINER_HEADER_INCLUDED

#include "art_deleters.h"
#include "basic_leaf.h"
#include "tree_iterator.h"

namespace art
{
namespace detail
{
// Forward declarations for node types
template <typename Db> class basic_inode_impl;
template <typename Db> class basic_inode_4;
template <typename Db> class basic_inode_16;
template <typename Db> class basic_inode_48;
template <typename Db> class basic_inode_256;

template <typename Traits> class db
{
public:
    using header_type = typename Traits::header_type;

private:
    using bitwise_key = typename Traits::bitwise_key;
    using multi_container = typename Traits::multi_container;
    using node_ptr = typename Traits::node_ptr;
    using fast_key_type = typename Traits::fast_key_type;

    // Leaves are always single-valued, so leaf count is the same as
    // the actual size of the container.
    using leaf_type =
        basic_leaf<header_type, typename Traits::mapped_type, typename Traits::allocator_type>;

    using leaf_allocator_type = typename Traits::allocator_type::template rebind<leaf_type>::other;
    using leaf_allocator_traits = std::allocator_traits<leaf_allocator_type>;

    using self_t = db<Traits>;
    using db_leaf_unique_ptr = basic_db_leaf_unique_ptr<leaf_type, self_t>;

    // We will be friends only with the nodes that have the same policy
    friend basic_inode_impl<self_t>;
    friend basic_inode_4<self_t>;
    friend basic_inode_16<self_t>;
    friend basic_inode_48<self_t>;
    friend basic_inode_256<self_t>;
    friend basic_leaf_deleter<leaf_type, self_t>;

public:
    using key_type = typename Traits::key_type;
    using mapped_type = typename Traits::mapped_type;
    using value_type = typename Traits::value_type;
    using pointer = typename Traits::pointer;
    using const_pointer = typename Traits::const_pointer;
    using reference = typename Traits::reference;
    using const_reference = typename Traits::const_reference;
    using size_type = typename Traits::size_type;
    using difference_type = typename Traits::difference_type;
    using key_compare = typename Traits::key_compare;
    using allocator_type = typename Traits::allocator_type;
    using iterator = tree_iterator<Traits, node_ptr>;
    using const_iterator = tree_iterator<const Traits, node_ptr>;
    using reverse_iterator = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

public:
    // Creation and destruction as defined in C++14 for std::map
    constexpr db() = default;

    explicit db(const allocator_type& alloc);

    template <class InputIt> db(InputIt first, InputIt last, const allocator_type& alloc);

    db(const db& other);
    db(const db& other, const allocator_type& alloc);
    db(db&& other);
    db(db&& other, const allocator_type& alloc);
    db(std::initializer_list<value_type> init, const allocator_type&);

    ~db() noexcept
    {
        if (!empty())
            delete_subtree(data.root);
    }

    // Allocator routines
    allocator_type get_allocator() const noexcept
    {
        return *static_cast<const allocator_type*>(&data);
    }

    // We are not using the given comparator, so the function here is a dummy one
    key_compare key_comp() const { return key_compare(); }

    // Iterator routines.
    iterator begin();
    const_iterator begin() const;
    iterator end();
    const_iterator end() const;
    reverse_iterator rbegin() { return reverse_iterator(end()); }
    const_reverse_iterator rbegin() const { return const_reverse_iterator(end()); }
    reverse_iterator rend() { return reverse_iterator(begin()); }
    const_reverse_iterator rend() const { return const_reverse_iterator(begin()); }

    size_type size() const noexcept { return leaf_count_; }
    bool empty() const noexcept { return data.root == nullptr; }

    // Lookup routines
    iterator find(fast_key_type key) noexcept { return internal_find(key).mutable_self(); }
    const_iterator find(fast_key_type key) const noexcept { return internal_find(key); }

    iterator lower_bound(fast_key_type key) noexcept;
    const_iterator lower_bound(fast_key_type key) const noexcept;
    template <class K> iterator lower_bound(const K& key);
    template <class K> const_iterator lower_bound(const K& key) const;

    iterator upper_bound(fast_key_type key);
    const_iterator upper_bound(fast_key_type key) const;
    template <class K> iterator upper_bound(const K& key);
    template <class K> const_iterator upper_bound(const K& key) const;

    std::pair<iterator, iterator> equal_range(fast_key_type key)
    {
        return std::make_pair(lower_bound(key), upper_bound(key));
    }
    std::pair<const_iterator, const_iterator> equal_range(fast_key_type key) const
    {
        return std::make_pair(lower_bound(key), upper_bound(key));
    }

    template <class K> std::pair<iterator, iterator> equal_range(const K& key)
    {
        return std::make_pair(lower_bound(key), upper_bound(key));
    }
    template <class K> std::pair<const_iterator, const_iterator> equal_range(const K& key) const
    {
        return std::make_pair(lower_bound(key), upper_bound(key));
    }

    // Insertion routines
    auto insert(const value_type& value)
    {
        return emplace_key_args(Traits::key(value), Traits::value(value));
    }
    auto insert(value_type&& value)
    {
        return emplace_key_args(Traits::key(value), Traits::value(std::move(value)));
    }
    template <class Arg> auto insert(Arg&& value)
    {
        return emplace_key_args(Traits::key(value), Traits::value(std::move(value)));
    }

    // Insert with hint. Check to see if the value should be placed immediately
    // before position in the tree. If it does, then the insertion will take
    // amortized constant time. If not, the insertion will take amortized
    // logarithmic time as if a call to insert(value) were made.
    iterator insert(iterator hint, const value_type& value)
    {
        return emplace_hint_key_args(hint, Traits::key(value), value);
    }
    iterator insert(iterator hint, value_type&& value)
    {
        return emplace_hint_key_args(hint, Traits::key(value), std::move(value));
    }
    template <typename Arg> iterator insert(iterator hint, Arg&& value)
    {
        return emplace_hint_key_args(hint, Traits::key(value), std::move(value));
    }

    size_type erase(fast_key_type key);
    iterator erase(iterator pos);
    iterator erase(iterator first, iterator last);

    void swap(self_t& other) noexcept;
    void clear();

    // Stats

    // Return current memory use by tree nodes in bytes.
    [[nodiscard]] constexpr size_type current_memory_use() const noexcept
    {
        return current_memory_use_;
    }

    [[nodiscard]] constexpr size_type leaf_count() const noexcept { return leaf_count_; }
    [[nodiscard]] constexpr size_type inode4_count() const noexcept { return inode4_count_; }
    [[nodiscard]] constexpr size_type inode16_count() const noexcept { return inode16_count_; }
    [[nodiscard]] constexpr size_type inode48_count() const noexcept { return inode48_count_; }
    [[nodiscard]] constexpr size_type inode256_count() const noexcept { return inode256_count_; }

    [[nodiscard]] constexpr std::uintmax_t created_inode4_count() const noexcept
    {
        return created_inode4_count_;
    }

    [[nodiscard]] constexpr std::uintmax_t inode4_to_inode16_count() const noexcept
    {
        return inode4_to_inode16_count_;
    }

    [[nodiscard]] constexpr std::uintmax_t inode16_to_inode48_count() const noexcept
    {
        return inode16_to_inode48_count_;
    }

    [[nodiscard]] constexpr std::uintmax_t inode48_to_inode256_count() const noexcept
    {
        return inode48_to_inode256_count_;
    }

    [[nodiscard]] constexpr std::uintmax_t deleted_inode4_count() const noexcept
    {
        return deleted_inode4_count_;
    }

    [[nodiscard]] constexpr std::uintmax_t inode16_to_inode4_count() const noexcept
    {
        return inode16_to_inode4_count_;
    }

    [[nodiscard]] constexpr std::uintmax_t inode48_to_inode16_count() const noexcept
    {
        return inode48_to_inode16_count_;
    }

    [[nodiscard]] constexpr std::uintmax_t inode256_to_inode48_count() const noexcept
    {
        return inode256_to_inode48_count_;
    }

    [[nodiscard]] constexpr std::uintmax_t key_prefix_splits() const noexcept
    {
        return key_prefix_splits_;
    }

    // Debugging
    void dump(std::ostream& os) const;

protected:
    template <typename... Args>
    [[nodiscard]] auto emplace_key_args(fast_key_type key, Args&&... args)
    {
        return emplace_key_args(multi_container(), key, std::forward<Args>(args)...);
    }

private:
    [[nodiscard]] const_iterator internal_locate(bitwise_key& key) const noexcept;
    [[nodiscard]] const_iterator internal_find(fast_key_type key) const noexcept;

    template <typename... Args>
    [[nodiscard]] iterator internal_emplace(const_iterator hint, bitwise_key key, Args&&... args);

    template <typename... Args>
    [[nodiscard]] iterator emplace_key_args(std::true_type, fast_key_type key, Args&&... args);

    template <typename... Args>
    [[nodiscard]] std::pair<iterator, bool> emplace_key_args(std::false_type, fast_key_type key,
                                                             Args&&... args);

    template <typename... Args>
    [[nodiscard]] iterator emplace_hint_key_args(iterator hint, fast_key_type key, Args&&... args);

private:
    [[nodiscard]] allocator_type& allocator() noexcept
    {
        return *static_cast<allocator_type*>(&data);
    }

    void delete_subtree(node_ptr) noexcept
    {
        // default_policy::delete_db_node_ptr_at_scope_exit delete_on_scope_exit(node, *this);
        // delete_on_scope_exit.delete_subtree();
    }

    constexpr void increase_memory_use(size_type delta) noexcept { current_memory_use_ += delta; }

    constexpr void decrease_memory_use(size_type delta) noexcept
    {
        assert(delta <= current_memory_use_);
        current_memory_use_ -= delta;
    }

    constexpr void increment_leaf_count() noexcept
    {
        increase_memory_use(sizeof(leaf_type));
        ++leaf_count_;
    }

    constexpr void decrement_leaf_count() noexcept
    {
        decrease_memory_use(sizeof(leaf_type));
        assert(leaf_count_ != 0);
        --leaf_count_;
    }

    // Leaf creation/deallocation
    [[nodiscard]] db_leaf_unique_ptr make_leaf_ptr(bitwise_key key);
    template <typename... Args>
    [[nodiscard]] db_leaf_unique_ptr make_leaf_ptr(bitwise_key key, Args&&... args);

    void deallocate(leaf_type* leaf) noexcept;

private:
    // A helper struct to get the empty base class optimization for 0 size allocators.
    // In C++20 parlance that would be [[no_unique_address]] for the allocator.
    struct compress_empty_base : public allocator_type {
        node_ptr root{nullptr};
    } data;

    size_type current_memory_use_{0};

    size_type leaf_count_{0};
    size_type inode4_count_{0};
    size_type inode16_count_{0};
    size_type inode48_count_{0};
    size_type inode256_count_{0};

    // While the counters above cannot exceed the size_type (after all, there are limits to a
    // physical machine), these counters may well reach the astronomical proportions during
    // the lifetime of this ART frontend.
    std::uintmax_t created_inode4_count_{0};
    std::uintmax_t inode4_to_inode16_count_{0};
    std::uintmax_t inode16_to_inode48_count_{0};
    std::uintmax_t inode48_to_inode256_count_{0};

    std::uintmax_t deleted_inode4_count_{0};
    std::uintmax_t inode16_to_inode4_count_{0};
    std::uintmax_t inode48_to_inode16_count_{0};
    std::uintmax_t inode256_to_inode48_count_{0};

    std::uintmax_t key_prefix_splits_{0};
};

} // namespace detail
} // namespace art

#include "art_container_impl.h"

#endif // ART_DETAIL_ART_CONTAINER_HEADER_INCLUDED
