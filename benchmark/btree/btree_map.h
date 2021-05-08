#ifndef UTIL_BTREE_BTREE_MAP_H__
#define UTIL_BTREE_BTREE_MAP_H__

#include "btree.h"
#include "btree_container.h"

namespace btree
{

// The btree_map class is needed mainly for its constructors.
template <typename Key, typename Value, typename Compare = std::less<Key>,
          typename Alloc = std::allocator<std::pair<const Key, Value>>,
          std::size_t TargetNodeSize = 256>
class btree_map
    : public btree_map_container<btree<map_parameters<Key, Value, Compare, Alloc, TargetNodeSize>>>
{

    typedef btree_map<Key, Value, Compare, Alloc, TargetNodeSize> self_type;
    typedef map_parameters<Key, Value, Compare, Alloc, TargetNodeSize> params_type;
    typedef btree<params_type> btree_type;
    typedef btree_map_container<btree_type> super_type;

public:
    using key_compare = typename btree_type::key_compare;
    using allocator_type = typename btree_type::allocator_type;

public:
    // Default constructor.
    btree_map(const key_compare& comp = key_compare(),
              const allocator_type& alloc = allocator_type())
        : super_type(comp, alloc)
    {
    }

    // Copy constructor.
    btree_map(const self_type& x)
        : super_type(x)
    {
    }

    // Range constructor.
    template <class InputIterator>
    btree_map(InputIterator b, InputIterator e, const key_compare& comp = key_compare(),
              const allocator_type& alloc = allocator_type())
        : super_type(b, e, comp, alloc)
    {
    }
};

template <typename K, typename V, typename C, typename A, int N>
inline void swap(btree_map<K, V, C, A, N>& x, btree_map<K, V, C, A, N>& y)
{
    x.swap(y);
}

// The btree_multimap class is needed mainly for its constructors.
template <typename Key, typename Value, typename Compare = std::less<Key>,
          typename Alloc = std::allocator<std::pair<const Key, Value>>,
          std::size_t TargetNodeSize = 256>
class btree_multimap : public btree_multi_container<
                           btree<map_parameters<Key, Value, Compare, Alloc, TargetNodeSize>>>
{

    typedef btree_multimap<Key, Value, Compare, Alloc, TargetNodeSize> self_type;
    typedef map_parameters<Key, Value, Compare, Alloc, TargetNodeSize> params_type;
    typedef btree<params_type> btree_type;
    typedef btree_multi_container<btree_type> super_type;

public:
    using key_compare = typename btree_type::key_compare;
    using allocator_type = typename btree_type::allocator_type;

public:
    // Default constructor.
    btree_multimap(const key_compare& comp = key_compare(),
                   const allocator_type& alloc = allocator_type())
        : super_type(comp, alloc)
    {
    }

    // Copy constructor.
    btree_multimap(const self_type& x)
        : super_type(x)
    {
    }

    // Range constructor.
    template <class InputIterator>
    btree_multimap(InputIterator b, InputIterator e, const key_compare& comp = key_compare(),
                   const allocator_type& alloc = allocator_type())
        : super_type(b, e, comp, alloc)
    {
    }
};

template <typename K, typename V, typename C, typename A, int N>
inline void swap(btree_multimap<K, V, C, A, N>& x, btree_multimap<K, V, C, A, N>& y)
{
    x.swap(y);
}

} // namespace btree

#endif // UTIL_BTREE_BTREE_MAP_H__
