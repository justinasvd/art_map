#ifndef UTIL_BTREE_BTREE_CONTAINER_H__
#define UTIL_BTREE_BTREE_CONTAINER_H__

#include "btree.h"

#include <tuple>

namespace btree
{

// A common base class for btree_set, btree_map, btree_multiset and
// btree_multimap.
template <typename Tree> class btree_container
{
    typedef btree_container<Tree> self_type;

    using fast_key_type = typename Tree::fast_key_type;

public:
    typedef typename Tree::params_type params_type;
    typedef typename Tree::key_type key_type;
    typedef typename Tree::value_type value_type;
    typedef typename Tree::key_compare key_compare;
    typedef typename Tree::allocator_type allocator_type;
    typedef typename Tree::pointer pointer;
    typedef typename Tree::const_pointer const_pointer;
    typedef typename Tree::reference reference;
    typedef typename Tree::const_reference const_reference;
    typedef typename Tree::size_type size_type;
    typedef typename Tree::difference_type difference_type;
    typedef typename Tree::iterator iterator;
    typedef typename Tree::const_iterator const_iterator;
    typedef typename Tree::reverse_iterator reverse_iterator;
    typedef typename Tree::const_reverse_iterator const_reverse_iterator;

public:
    // Default constructor.
    btree_container(const key_compare& comp, const allocator_type& alloc)
        : tree_(comp, alloc)
    {
    }

    // Copy constructor.
    btree_container(const self_type& x)
        : tree_(x.tree_)
    {
    }

    // Iterator routines.
    iterator begin() { return tree_.begin(); }
    const_iterator begin() const { return tree_.begin(); }
    iterator end() { return tree_.end(); }
    const_iterator end() const { return tree_.end(); }
    reverse_iterator rbegin() { return tree_.rbegin(); }
    const_reverse_iterator rbegin() const { return tree_.rbegin(); }
    reverse_iterator rend() { return tree_.rend(); }
    const_reverse_iterator rend() const { return tree_.rend(); }

    // Lookup routines.
    iterator lower_bound(fast_key_type key) { return tree_.lower_bound(key); }
    const_iterator lower_bound(fast_key_type key) const { return tree_.lower_bound(key); }
    iterator upper_bound(fast_key_type key) { return tree_.upper_bound(key); }
    const_iterator upper_bound(fast_key_type key) const { return tree_.upper_bound(key); }
    std::pair<iterator, iterator> equal_range(fast_key_type key) { return tree_.equal_range(key); }
    std::pair<const_iterator, const_iterator> equal_range(fast_key_type key) const
    {
        return tree_.equal_range(key);
    }

    // Utility routines.
    void clear() { tree_.clear(); }
    void swap(self_type& x) { tree_.swap(x.tree_); }
    void dump(std::ostream& os) const { tree_.dump(os); }
    void verify() const { tree_.verify(); }

    // Size routines.
    size_type size() const { return tree_.size(); }
    size_type max_size() const { return tree_.max_size(); }
    bool empty() const { return tree_.empty(); }
    size_type height() const { return tree_.height(); }
    size_type internal_nodes() const { return tree_.internal_nodes(); }
    size_type leaf_nodes() const { return tree_.leaf_nodes(); }
    size_type nodes() const { return tree_.nodes(); }
    size_type bytes_used() const { return tree_.bytes_used(); }
    static double average_bytes_per_value() noexcept { return Tree::average_bytes_per_value(); }
    double fullness() const { return tree_.fullness(); }
    double overhead() const { return tree_.overhead(); }

protected:
    Tree tree_;
};

template <typename T>
inline bool operator==(const btree_container<T>& lhs, const btree_container<T>& rhs)
{
    return (lhs.size() == rhs.size()) && std::equal(lhs.begin(), lhs.end(), rhs.begin());
}
template <typename T>
inline bool operator!=(const btree_container<T>& lhs, const btree_container<T>& rhs)
{
    return !(lhs == rhs);
}

template <typename T> inline std::ostream& operator<<(std::ostream& os, const btree_container<T>& b)
{
    b.dump(os);
    return os;
}

// A common base class for btree_set and safe_btree_set.
template <typename Tree> class btree_unique_container : public btree_container<Tree>
{
    typedef btree_unique_container<Tree> self_type;
    typedef btree_container<Tree> super_type;

    using fast_key_type = typename Tree::fast_key_type;

public:
    typedef typename Tree::key_type key_type;
    typedef typename Tree::value_type value_type;
    typedef typename Tree::size_type size_type;
    typedef typename Tree::key_compare key_compare;
    typedef typename Tree::allocator_type allocator_type;
    typedef typename Tree::iterator iterator;
    typedef typename Tree::const_iterator const_iterator;

public:
    // Default constructor.
    btree_unique_container(const key_compare& comp = key_compare(),
                           const allocator_type& alloc = allocator_type())
        : super_type(comp, alloc)
    {
    }

    // Copy constructor.
    btree_unique_container(const self_type& x)
        : super_type(x)
    {
    }

    // Range constructor.
    template <class InputIterator>
    btree_unique_container(InputIterator b, InputIterator e,
                           const key_compare& comp = key_compare(),
                           const allocator_type& alloc = allocator_type())
        : super_type(comp, alloc)
    {
        insert(b, e);
    }

    // Lookup routines.
    iterator find(fast_key_type key) { return this->tree_.find_unique(key); }
    const_iterator find(fast_key_type key) const { return this->tree_.find_unique(key); }
    size_type count(fast_key_type key) const { return this->tree_.count_unique(key); }

    // Insertion routines.
    std::pair<iterator, bool> insert(const value_type& value)
    {
        return this->tree_.insert_unique(value);
    }
    template <class P> std::pair<iterator, bool> insert(P&& value)
    {
        return this->tree_.insert_unique(std::move(value));
    }
    std::pair<iterator, bool> insert(value_type&& value)
    {
        return this->tree_.insert_unique(std::move(value));
    }
    iterator insert(const_iterator hint, const value_type& x)
    {
        return this->tree_.insert_unique(hint, x);
    }
    template <class P> iterator insert(const_iterator hint, P&& value)
    {
        return this->tree_.insert_unique(hint, std::move(value));
    }
    iterator insert(const_iterator hint, value_type&& x)
    {
        return this->tree_.insert_unique(hint, std::move(x));
    }
    template <typename InputIterator> void insert(InputIterator first, InputIterator last)
    {
        this->tree_.insert_unique(first, last);
    }

    // Deletion routines.
    size_type erase(fast_key_type key) { return this->tree_.erase_unique(key); }
    // Erase the specified iterator from the btree. The iterator must be valid
    // (i.e. not equal to end()).  Return an iterator pointing to the node after
    // the one that was erased (or end() if none exists).
    iterator erase(const iterator& iter) { return this->tree_.erase(iter); }
    void erase(const iterator& first, const iterator& last) { this->tree_.erase(first, last); }
};

// A common base class for btree_map and safe_btree_map.
template <typename Tree> class btree_map_container : public btree_unique_container<Tree>
{
    typedef btree_map_container<Tree> self_type;
    typedef btree_unique_container<Tree> super_type;

    using fast_key_type = typename Tree::fast_key_type;

public:
    using key_type = typename Tree::key_type;
    using data_type = typename Tree::data_type;
    using value_type = typename Tree::value_type;
    using mapped_type = typename Tree::mapped_type;
    using key_compare = typename Tree::key_compare;
    using allocator_type = typename Tree::allocator_type;
    using iterator = typename Tree::iterator;
    using const_iterator = typename Tree::const_iterator;

public:
    // Default constructor.
    btree_map_container(const key_compare& comp = key_compare(),
                        const allocator_type& alloc = allocator_type())
        : super_type(comp, alloc)
    {
    }

    // Copy constructor.
    btree_map_container(const self_type& x)
        : super_type(x)
    {
    }

    // Range constructor.
    template <class InputIterator>
    btree_map_container(InputIterator b, InputIterator e, const key_compare& comp = key_compare(),
                        const allocator_type& alloc = allocator_type())
        : super_type(b, e, comp, alloc)
    {
    }

    template <typename... Args> std::pair<iterator, bool> emplace(fast_key_type key, Args&&... args)
    {
        return this->tree_.emplace_unique_key_args(
            key, std::piecewise_construct, std::forward_as_tuple(key),
            std::forward_as_tuple(std::forward<Args>(args)...));
    }

    template <typename... Args> std::pair<iterator, bool> emplace(key_type&& key, Args&&... args)
    {
        return this->tree_.emplace_unique_key_args(
            key, std::piecewise_construct, std::forward_as_tuple(std::move(key)),
            std::forward_as_tuple(std::forward<Args>(args)...));
    }

    template <typename... Args>
    iterator emplace_hint(const_iterator hint, fast_key_type key, Args&&... args)
    {
        return this->tree_.emplace_hint_unique_key_args(
            hint, key, std::piecewise_construct, std::forward_as_tuple(key),
            std::forward_as_tuple(std::forward<Args>(args)...));
    }

    template <typename... Args>
    std::pair<iterator, bool> try_emplace(fast_key_type key, Args&&... args)
    {
        return this->tree_.emplace_unique_key_args(
            key, std::piecewise_construct, std::forward_as_tuple(key),
            std::forward_as_tuple(std::forward<Args>(args)...));
    }

    template <typename... Args>
    std::pair<iterator, bool> try_emplace(key_type&& key, Args&&... args)
    {
        return this->tree_.emplace_unique_key_args(
            key, std::piecewise_construct, std::forward_as_tuple(std::move(key)),
            std::forward_as_tuple(std::forward<Args>(args)...));
    }

    template <typename... Args>
    iterator try_emplace(const_iterator hint, fast_key_type key, Args&&... args)
    {
        return this->tree_.emplace_hint_unique_key_args(
            hint, key, std::piecewise_construct, std::forward_as_tuple(key),
            std::forward_as_tuple(std::forward<Args>(args)...));
    }

    template <typename... Args>
    iterator try_emplace(const_iterator hint, key_type&& key, Args&&... args)
    {
        return this->tree_.emplace_hint_unique_key_args(
            hint, key, std::piecewise_construct, std::forward_as_tuple(std::move(key)),
            std::forward_as_tuple(std::forward<Args>(args)...));
    }

    // Access specified element with bounds checking.
    mapped_type& at(fast_key_type key)
    {
        auto it = this->find(key);
        if (it == this->end()) {
            throw std::out_of_range("map::at: key not found");
        }
        return it->second;
    }
    const mapped_type& at(fast_key_type key) const
    {
        auto it = this->find(key);
        if (it == this->end()) {
            throw std::out_of_range("map::at: key not found");
        }
        return it->second;
    }

    // Insertion routines.
    data_type& operator[](fast_key_type key) { return this->try_emplace(key).first->second; }
    data_type& operator[](key_type&& key)
    {
        return this->try_emplace(std::move(key)).first->second;
    }
};

// A common base class for btree_multiset and btree_multimap.
template <typename Tree> class btree_multi_container : public btree_container<Tree>
{
    typedef btree_multi_container<Tree> self_type;
    typedef btree_container<Tree> super_type;

    using fast_key_type = typename Tree::fast_key_type;

public:
    typedef typename Tree::key_type key_type;
    typedef typename Tree::value_type value_type;
    typedef typename Tree::size_type size_type;
    typedef typename Tree::key_compare key_compare;
    typedef typename Tree::allocator_type allocator_type;
    typedef typename Tree::iterator iterator;
    typedef typename Tree::const_iterator const_iterator;

public:
    // Default constructor.
    btree_multi_container(const key_compare& comp = key_compare(),
                          const allocator_type& alloc = allocator_type())
        : super_type(comp, alloc)
    {
    }

    // Copy constructor.
    btree_multi_container(const self_type& x)
        : super_type(x)
    {
    }

    // Range constructor.
    template <class InputIterator>
    btree_multi_container(InputIterator b, InputIterator e, const key_compare& comp = key_compare(),
                          const allocator_type& alloc = allocator_type())
        : super_type(comp, alloc)
    {
        insert(b, e);
    }

    // Lookup routines.
    iterator find(fast_key_type key) { return this->tree_.find_multi(key); }
    const_iterator find(fast_key_type key) const { return this->tree_.find_multi(key); }
    size_type count(fast_key_type key) const { return this->tree_.count_multi(key); }

    // Insertion routines.
    iterator insert(const value_type& value) { return this->tree_.insert_multi(value); }
    template <class P> iterator insert(P&& value)
    {
        return this->tree_.insert_multi(std::move(value));
    }
    iterator insert(value_type&& value) { return this->tree_.insert_multi(std::move(value)); }
    iterator insert(const_iterator hint, const value_type& x)
    {
        return this->tree_.insert_multi(hint, x);
    }
    template <class P> iterator insert(const_iterator hint, P&& value)
    {
        return this->tree_.insert_multi(hint, std::move(value));
    }
    iterator insert(const_iterator hint, value_type&& x)
    {
        return this->tree_.insert_multi(hint, std::move(x));
    }
    template <typename InputIterator> void insert(InputIterator first, InputIterator last)
    {
        this->tree_.insert_multi(first, last);
    }

    // Deletion routines.
    size_type erase(fast_key_type key) { return this->tree_.erase_multi(key); }
    // Erase the specified iterator from the btree. The iterator must be valid
    // (i.e. not equal to end()).  Return an iterator pointing to the node after
    // the one that was erased (or end() if none exists).
    iterator erase(const iterator& iter) { return this->tree_.erase(iter); }
    void erase(const iterator& first, const iterator& last) { this->tree_.erase(first, last); }
};

} // namespace btree

#endif // UTIL_BTREE_BTREE_CONTAINER_H__
