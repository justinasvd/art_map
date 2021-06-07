#ifndef ART_DETAIL_ART_CONTAINER_HEADER_INCLUDED
#define ART_DETAIL_ART_CONTAINER_HEADER_INCLUDED

#include "art_deleter.h"
#include "tree_iterator.h"

#include <cassert>

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
    using bitwise_key = typename Traits::bitwise_key;

private:
    using multi_container = typename Traits::multi_container;
    using node_base = typename Traits::node_base;
    using node_ptr = node_base*;
    using fast_key_type = typename Traits::fast_key_type;
    using leaf_type = typename Traits::leaf_type;

    using self_t = db<Traits>;
    using leaf_unique_ptr = unique_node_ptr<leaf_type, self_t>;

    using inode = basic_inode_impl<self_t>;
    using inode_4 = basic_inode_4<self_t>;
    using inode_16 = basic_inode_16<self_t>;
    using inode_48 = basic_inode_48<self_t>;
    using inode_256 = basic_inode_256<self_t>;

    // We will be friends only with the nodes that have the same policy
    friend inode;
    friend inode_4;
    friend inode_16;
    friend inode_48;
    friend inode_256;

    // Make deleters friends
    friend node_deleter<inode_4, self_t>;
    friend node_deleter<inode_16, self_t>;
    friend node_deleter<inode_48, self_t>;
    friend node_deleter<inode_256, self_t>;
    friend node_deleter<node_base, self_t>;
    friend node_deleter<leaf_type, self_t>;

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
    using iterator = tree_iterator<Traits, node_base, inode>;
    using const_iterator = tree_iterator<const Traits, node_base, inode>;
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

    ~db() noexcept(std::is_nothrow_destructible<mapped_type>::value)
    {
        if (!empty())
            delete_subtree(tree.root);
    }

    // Allocator routines
    allocator_type get_allocator() const noexcept
    {
        return *static_cast<const allocator_type*>(&tree);
    }

    // We are not using the given comparator, so the function here is a dummy one
    key_compare key_comp() const { return key_compare(); }

    // Iterator routines.
    iterator begin() noexcept { return iterator(cbegin()); };
    const_iterator cbegin() const noexcept { return inode::leftmost_leaf(tree.root); }
    const_iterator begin() const noexcept { return cbegin(); }

    iterator end() noexcept { return iterator(tree.root, iterator::is_leaf(tree.root)); }
    const_iterator cend() const noexcept
    {
        return const_iterator(tree.root, iterator::is_leaf(tree.root));
    }
    const_iterator end() const noexcept { return cend(); }

    reverse_iterator rbegin() noexcept { return reverse_iterator(end()); }
    const_reverse_iterator rbegin() const noexcept { return const_reverse_iterator(end()); }
    reverse_iterator rend() { return reverse_iterator(begin()); }
    const_reverse_iterator rend() const noexcept { return const_reverse_iterator(begin()); }

    size_type size() const noexcept { return leaf_count(); }
    bool empty() const noexcept { return tree.root == nullptr; }

    // Lookup routines
    iterator find(fast_key_type key) noexcept { return iterator(internal_find(key)); }
    const_iterator find(fast_key_type key) const noexcept { return internal_find(key); }

    iterator lower_bound(fast_key_type key) noexcept;
    const_iterator lower_bound(fast_key_type key) const noexcept;
    template <class K> iterator lower_bound(const K& key);
    template <class K> const_iterator lower_bound(const K& key) const;

    iterator upper_bound(fast_key_type key);
    const_iterator upper_bound(fast_key_type key) const;
    template <class K> iterator upper_bound(const K& key);
    template <class K> const_iterator upper_bound(const K& key) const;

    // Since C++20
    bool contains(fast_key_type key) const noexcept
    {
        auto bitk = make_bitwise_key_prefix(key);
        return internal_locate(bitk).match(bitk.first);
    }
    template <class K> bool contains(const K& key) const;

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
        return emplace_hint_key_args(hint, Traits::key(value), Traits::value(value));
    }
    iterator insert(iterator hint, value_type&& value)
    {
        return emplace_hint_key_args(hint, Traits::key(value), Traits::value(std::move(value)));
    }
    template <typename Arg> iterator insert(iterator hint, Arg&& value)
    {
        return emplace_hint_key_args(hint, Traits::key(value), Traits::value(std::move(value)));
    }

    template <typename InputIterator> void insert(InputIterator first, InputIterator last)
    {
        for (; first != last; ++first)
            insert(*first);
    }

    size_type erase(fast_key_type key);
    iterator erase(iterator pos) { return pos != end() ? std::next(internal_erase(pos)) : pos; }
    iterator erase(iterator first, iterator last);

    void swap(self_t& other) noexcept;
    void clear();

    // Stats

    // Return current memory use by tree nodes in bytes.
    [[nodiscard]] constexpr size_type current_memory_use() const noexcept
    {
        return memory_use<leaf_type>() + memory_use<inode_4>() + memory_use<inode_16>() +
               memory_use<inode_256>();
    }

    [[nodiscard]] constexpr size_type leaf_count() const noexcept { return get_count<leaf_type>(); }
    [[nodiscard]] constexpr size_type inode4_count() const noexcept { return get_count<inode_4>(); }
    [[nodiscard]] constexpr size_type inode16_count() const noexcept
    {
        return get_count<inode_16>();
    }
    [[nodiscard]] constexpr size_type inode48_count() const noexcept
    {
        return get_count<inode_48>();
    }
    [[nodiscard]] constexpr size_type inode256_count() const noexcept
    {
        return get_count<inode_256>();
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
    using key_size_type = typename bitwise_key::size_type;
    using bitwise_key_prefix = std::pair<bitwise_key, key_size_type>;

    [[nodiscard]] static constexpr bitwise_key_prefix make_bitwise_key_prefix(
        fast_key_type key) noexcept
    {
        bitwise_key bitk(key);
        return std::make_pair(bitk, bitk.max_size());
    }

    [[nodiscard]] const_iterator internal_locate(bitwise_key_prefix& key) const noexcept;
    [[nodiscard]] const_iterator internal_find(fast_key_type key) const noexcept;

    template <typename... Args>
    [[nodiscard]] iterator internal_emplace(const_iterator hint, fast_key_type original_key,
                                            const bitwise_key_prefix& key, Args&&... args);

    template <typename... Args>
    [[nodiscard]] iterator emplace_key_args(std::true_type, fast_key_type key, Args&&... args);

    template <typename... Args>
    [[nodiscard]] std::pair<iterator, bool> emplace_key_args(std::false_type, fast_key_type key,
                                                             Args&&... args);

    template <typename... Args>
    [[nodiscard]] iterator emplace_hint_key_args(iterator hint, fast_key_type key, Args&&... args);

    iterator internal_erase(iterator pos);

private:
    [[nodiscard]] allocator_type& allocator() noexcept
    {
        return *static_cast<allocator_type*>(&tree);
    }

    template <typename Node> [[nodiscard]] constexpr size_type get_count() const noexcept
    {
        return std::get<counter<Node>>(count_).instances;
    }

    template <typename Node> [[nodiscard]] constexpr size_type memory_use() const noexcept
    {
        return sizeof(Node) * get_count<Node>();
    }

    template <typename Node> [[nodiscard]] constexpr size_type& count() noexcept
    {
        return std::get<counter<Node>>(count_).instances;
    }

    static constexpr inode* inode_cast(node_ptr node) noexcept { return static_cast<inode*>(node); }

    template <typename Node> unique_node_ptr<Node, self_t> make_unique_node_ptr(Node* node) noexcept
    {
        assert(node != nullptr);
        using ptr_type = unique_node_ptr<Node, self_t>;
        return ptr_type(node, node_deleter<Node, self_t>(*this));
    }

    void delete_subtree(node_ptr node) noexcept
    {
        auto delete_on_scope_exit = make_unique_node_ptr(node);
        if (node->type() != node_type::LEAF)
            inode_cast(node)->delete_subtree(*this);
    }

    template <typename Node, typename... Args>
    unique_node_ptr<Node, self_t> make_node_ptr(Args&&... args);

    // Leaf creation/deallocation
    template <typename... Args>
    [[nodiscard]] leaf_unique_ptr make_leaf_ptr(fast_key_type key, Args&&... args);

    template <typename Node> void deallocate_node(Node* node) noexcept;
    void deallocate(inode_4* node) noexcept { deallocate_node(node); }
    void deallocate(inode_16* node) noexcept { deallocate_node(node); }
    void deallocate(inode_48* node) noexcept { deallocate_node(node); }
    void deallocate(inode_256* node) noexcept { deallocate_node(node); }
    void deallocate(leaf_type* leaf) noexcept(std::is_nothrow_destructible<mapped_type>::value);
    void deallocate(node_ptr node) noexcept(std::is_nothrow_destructible<mapped_type>::value);

    template <typename NodePtr> void release_to_parent(const_iterator hint, NodePtr child) noexcept;

    template <typename NodePtr>
    iterator create_inode_4(const_iterator hint, bitwise_key prefix, NodePtr pdst,
                            leaf_unique_ptr leaf, key_size_type rem);

    template <typename Source>
    iterator grow_node(const_iterator hint, node_ptr dest_node, leaf_unique_ptr leaf,
                       std::uint8_t key_byte);

    // Functions to convert a node to a smaller type of node. This allows to fully
    // generalize the shrinking routine without stooping to strange hacks.
    template <typename Node>
    [[nodiscard]] unique_node_ptr<typename Node::smaller_inode_type, self_t> make_smaller_node(
        Node& src, std::uint8_t child_to_delete)
    {
        return make_node_ptr<typename Node::smaller_inode_type>(src, child_to_delete);
    }

    [[nodiscard]] unique_node_ptr<node_base, self_t> make_smaller_node(
        inode_4& src, std::uint8_t child_to_delete) noexcept
    {
        return src.leave_last_child(child_to_delete, *this);
    }

    template <typename Source> iterator shrink_node(iterator pos);

private:
    // A helper struct to get the empty base class optimization for 0 size allocators.
    // In C++20 parlance that would be [[no_unique_address]] for the allocator.
    struct compress_empty_base : public allocator_type {
        node_ptr root{nullptr};
    } tree;

    template <typename T> struct counter {
        size_type instances;
    };
    using node_stats_type = std::tuple<counter<leaf_type>, counter<inode_4>, counter<inode_16>,
                                       counter<inode_48>, counter<inode_256>>;
    node_stats_type count_{};
};

} // namespace detail
} // namespace art

#include "art_container_impl.h"

#endif // ART_DETAIL_ART_CONTAINER_HEADER_INCLUDED
