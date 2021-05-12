// A btree implementation of the STL set and map interfaces. A btree is both
// smaller and faster than STL set/map. The red-black tree implementation of
// STL set/map has an overhead of 3 pointers (left, right and parent) plus the
// node color information for each stored value. So a set<int32> consumes 20
// bytes for each value stored. This btree implementation stores multiple
// values on fixed size nodes (usually 256 bytes) and doesn't store child
// pointers for leaf nodes. The result is that a btree_set<int32> may use much
// less memory per stored value. For the random insertion benchmark in
// btree_test.cc, a btree_set<int32> with node-size of 256 uses 4.9 bytes per
// stored value.
//
// The packing of multiple values on to each node of a btree has another effect
// besides better space utilization: better cache locality due to fewer cache
// lines being accessed. Better cache locality translates into faster
// operations.
//
// CAVEATS
//
// Insertions and deletions on a btree can cause splitting, merging or
// rebalancing of btree nodes. And even without these operations, insertions
// and deletions on a btree will move values around within a node. In both
// cases, the result is that insertions and deletions can invalidate iterators
// pointing to values other than the one being inserted/deleted. This is
// notably different from STL set/map which takes care to not invalidate
// iterators on insert/erase except, of course, for iterators pointing to the
// value being erased.  A partial workaround when erasing is available:
// erase() returns an iterator pointing to the item just after the one that was
// erased (or end() if none exists). See also safe_btree.

#ifndef BTREE_MAP_DETAIL_BTREE_H
#define BTREE_MAP_DETAIL_BTREE_H

#include <algorithm>
#include <array>
#include <cassert>
#include <iterator>
#include <limits>
#include <ostream>
#include <string>
#include <type_traits>
#include <utility>

namespace btree
{

namespace detail
{

// Inside a btree method, if we just call swap(), it will choose the
// btree::swap method, which we don't want. And we can't say ::swap
// because then MSVC won't pickup any std::swap() implementations. We
// can't just use std::swap() directly because then we don't get the
// specialization for types outside the std namespace. So the solution
// is to have a special swap helper function whose name doesn't
// collide with other swap functions defined by the btree classes.
template <typename T>
inline void swap_helper(T& a, T& b) noexcept(
    std::is_nothrow_move_constructible<T>::value&& std::is_nothrow_move_assignable<T>::value)
{
    using std::swap;
    swap(a, b);
}

template <typename ResultT> using is_3way_comparer = std::is_signed<ResultT>;

template <typename Compare, typename Key>
using is_3way_comparer_fn = is_3way_comparer<decltype(Compare()(Key(), Key()))>;

template <typename String> struct string_less {
    int operator()(const String& a, const String& b) const noexcept { return a.compare(b); }
};

template <typename String> struct string_greater {
    int operator()(const String& a, const String& b) const noexcept { return b.compare(a); }
};

// A comparer when a three-way comparison is present
template <typename Key> struct neg_compare {
    template <typename Compare> static bool apply(const Compare& comp, Key x, Key y)
    {
        return comp(x, y) < 0;
    }
};

template <typename Key> struct bool_compare {
    template <typename Compare> static bool apply(const Compare& comp, Key x, Key y)
    {
        return comp(x, y);
    }
};

// Fast argument type
template <typename T> struct fast_const_argument {
    using const_reference = std::add_lvalue_reference_t<std::add_const_t<T>>;

    // Sufficiently small integral and floating point types are passed by value
    using type =
        std::conditional_t<(std::is_integral<T>::value || std::is_floating_point<T>::value) &&
                               sizeof(T) <= sizeof(const_reference),
                           T, const_reference>;
};

template <typename T> using fast_const_argument_t = typename fast_const_argument<T>::type;

} // namespace detail

// A helper class to register a three-way comparison that returns
// a negative value to indicate less-than, zero to indicate equality
// and a positive value to indicate greater-than. This helper class
// is already specialized for less<string> and greater<string>.
//
//  struct MyStringComparer {
//    int operator()(const string& a, const string& b) const {
//      return a.compare(b);
//    }
//  };
//
//  template <> struct three_way_comparison<MyStringComparer> {
//    using type = MyStringComparer;
// };
//
// Note that the return type can be either an int or a bool. There is a
// static_assert which enforces these return types.
template <typename Compare> struct three_way_comparison {
    using type = Compare;
};

// Three way comparison specializations for strings
template <class CharT, class Traits, class Allocator>
struct three_way_comparison<std::less<std::basic_string<CharT, Traits, Allocator>>> {
    using type = detail::string_less<std::basic_string<CharT, Traits, Allocator>>;
};

template <class CharT, class Traits, class Allocator>
struct three_way_comparison<std::greater<std::basic_string<CharT, Traits, Allocator>>> {
    using type = detail::string_greater<std::basic_string<CharT, Traits, Allocator>>;
};

template <typename Key, typename Value, typename Compare, typename Alloc,
          std::size_t TargetNodeSize>
struct common_parameters {
    // Leaf overhead is no fewer than 2 pointers
    static constexpr std::size_t leaf_overhead = 2 * sizeof(void*);
    static constexpr std::size_t target_node_size = TargetNodeSize;

    // We need a minimum of 3 values per internal node in order to perform
    // splitting (1 value for the two nodes involved in the split and 1 value
    // propagated to the parent as the delimiter for the split).
    static_assert(target_node_size >= (leaf_overhead + 3 * sizeof(Value)),
                  "Target node size is too small");

    // If three_way_comparison has a specialization for Compare, then use it as the
    // key_compare type. Otherwise, fall-back to Compare if we don't have an appropriate
    // specialization.
    using key_compare = typename three_way_comparison<Compare>::type;

    using key_type = Key;
    using value_type = Value;
    using pointer = value_type*;
    using const_pointer = const value_type*;
    using reference = value_type&;
    using const_reference = const value_type&;
    using size_type = ssize_t; // Size type signed? Questionable!
    using difference_type = std::ptrdiff_t;
    using allocator_type = Alloc;

    // This is an integral type large enough to hold as many values as will fit a node of
    // target_node_size bytes.
    using node_count_type =
        std::conditional_t<((target_node_size - leaf_overhead) / sizeof(Value)) >= 256,
                           std::uint16_t, std::uint8_t>;
};

// A parameters structure for holding the type parameters for a btree_map.
template <typename Key, typename Data, typename Compare, typename Alloc, int TargetNodeSize>
struct map_parameters
    : public common_parameters<Key, std::pair<const Key, Data>, Compare, Alloc, TargetNodeSize> {
    using data_type = Data;
    using mapped_type = Data;
    using mutable_value_type = std::pair<Key, Data>;

    using fast_key_type = detail::fast_const_argument_t<Key>;

    // We don't really care what kind of Pair we get here. The biggest requirement is not to
    // make any temporaries (!). So long that the first member of the pair is convertible to
    // fast_key_type, we're all good.
    template <typename Pair> static fast_key_type key(const Pair& x) noexcept { return x.first; }
};

// A parameters structure for holding the type parameters for a btree_set.
template <typename Key, typename Compare, typename Alloc, int TargetNodeSize>
struct set_parameters : public common_parameters<Key, Key, Compare, Alloc, TargetNodeSize> {
    using data_type = std::false_type;
    using mapped_type = std::false_type;
    using mutable_value_type = Key;

    using fast_key_type = detail::fast_const_argument_t<Key>;
    static fast_key_type key(fast_key_type x) noexcept { return x; }
};

// Return type for btree search routines. The precise return type is selected
// depending on the kind of comparator we have.
enum class match_kind : std::uint8_t { exact, not_exact };

template <typename T, bool IsCompareTo> struct search_result {
    T value;
    match_kind match;

    static constexpr bool has_exact_match() noexcept { return true; }
    bool is_exact_match() const noexcept { return match == match_kind::exact; }
};

// When we don't use CompareTo, `match` is not present.
// This ensures that callers can't use it accidentally when it provides no
// useful information.
template <typename T> struct search_result<T, false> {
    T value;

    static constexpr bool has_exact_match() noexcept { return false; }
    static constexpr bool is_exact_match() noexcept { return false; }
};

// A node in the btree holding. The same node type is used for both internal
// and leaf nodes in the btree, though the nodes are allocated in such a way
// that the children array is only valid in internal nodes.
template <typename Params> class btree_node
{
    using self_type = btree_node<Params>;
    using mutable_value_type = typename Params::mutable_value_type;

public:
    using key_type = typename Params::key_type;
    using data_type = typename Params::data_type;
    using value_type = typename Params::value_type;
    using pointer = typename Params::pointer;
    using const_pointer = typename Params::const_pointer;
    using reference = typename Params::reference;
    using const_reference = typename Params::const_reference;
    using key_compare = typename Params::key_compare;
    using size_type = typename Params::size_type;
    using difference_type = typename Params::difference_type;

    using allocator_type = typename Params::allocator_type;

    using fast_key_type = typename Params::fast_key_type;

    // Catch strange size mismatches (very unlikely, just being paranoid here)
    static_assert(sizeof(value_type) == sizeof(mutable_value_type),
                  "Size mismatch between mutable and actual value types");

private:
    // A type which indicates if we have a key-compare-to functor or a plain old
    // key-compare functor.
    using is_3way_comparer = detail::is_3way_comparer_fn<key_compare, key_type>;
    using field_type = typename Params::node_count_type;

    static constexpr bool is_nothrow_destructible =
        std::is_nothrow_destructible<mutable_value_type>::value;

    static constexpr bool is_nothrow_movable =
        std::is_nothrow_move_constructible<mutable_value_type>::value;

    static constexpr bool is_nothrow_assignable =
        std::is_nothrow_move_assignable<mutable_value_type>::value;

    static constexpr bool is_nothrow_operation = is_nothrow_assignable && is_nothrow_destructible;

public:
    struct base_fields {
        // The position of the node in the node's parent.
        field_type position;
        // The maximum number of values the node can hold. This is an integer in
        // [1, kNodeValues] for root leaf nodes, kNodeValues for non-root leaf
        // nodes, and kInternalNodeMaxCount (as a sentinel value) for internal
        // nodes (even though there are still kNodeValues values in the node).
        field_type max_count;
        // The count of the number of values in the node.
        field_type count;
        // A pointer to the node's parent.
        btree_node* parent;
    };

    // Test the assumption made when choosing node_count_type.
    static_assert(sizeof(base_fields) <= Params::leaf_overhead, "Node space assumption incorrect");

    static constexpr std::size_t target_node_size = Params::target_node_size;

    enum {
        // Compute how many values we can fit onto a leaf node.
        kNodeValues = (target_node_size - sizeof(base_fields)) / sizeof(value_type),

        // The node is internal (i.e. is not a leaf node) if and only if `max_count`
        // has this value.
        kInternalNodeMaxCount = 0,
    };

    // Can we form a valid tree?
    static_assert(kNodeValues >= 3, "Not enough space for node values");

    // Verify that we won't have more node values than we can address using the field_type
    static_assert(kNodeValues < std::numeric_limits<field_type>::max(),
                  "Target node size too large");

    // If the key is an integral or floating point type, use linear search for small enough
    // value types, which is faster than binary search for such types.
    using is_linear_search = std::integral_constant<
        bool, (kNodeValues <= 128) && (sizeof(value_type) <= sizeof(pointer)) &&
                  (std::is_integral<key_type>::value || std::is_floating_point<key_type>::value)>;

    struct leaf_fields : public base_fields {
        // The array of values. Only the first count of these values have been
        // constructed and are valid.
        mutable_value_type values[kNodeValues];
    };

    struct internal_fields : public leaf_fields {
        // The array of child pointers. The keys in children_[i] are all less than
        // key(i). The keys in children_[i + 1] are all greater than key(i). There
        // are always count + 1 children.
        std::array<btree_node*, kNodeValues + 1> children;
    };

    struct root_fields : public internal_fields {
        btree_node* rightmost;
        size_type size;
    };

public:
    // Whether this is a leaf node or not. This value doesn't change after the node is created.
    bool is_leaf() const noexcept
    {
        return fields_.max_count != static_cast<field_type>(kInternalNodeMaxCount);
    }

    // Getter for the position of this node in its parent.
    int position() const noexcept { return fields_.position; }

    // Getter/setter for the number of values stored in this node.
    int count() const noexcept { return fields_.count; }
    int max_count() const noexcept
    {
        // Leaf nodes have max_count in [1, kNodeValues].
        // Internal nodes have strictly kNodeValues.
        return is_leaf() ? fields_.max_count : static_cast<field_type>(kNodeValues);
    }

    // Getter for the parent of this node.
    btree_node* parent() const noexcept { return fields_.parent; }
    // Getter for whether the node is the root of the tree. The parent of the
    // root of the tree is the leftmost node in the tree which is guaranteed to
    // be a leaf.
    bool is_root() const noexcept { return parent()->is_leaf(); }
    void make_root() noexcept
    {
        assert(parent()->is_root());
        fields_.parent = fields_.parent->parent();
    }

    // Getter for the rightmost root node field. Only valid on the root node.
    btree_node* rightmost() const { return fields_.rightmost; }
    btree_node** mutable_rightmost() { return &fields_.rightmost; }

    // Getter for the size root node field. Only valid on the root node.
    size_type size() const noexcept { return fields_.size; }
    size_type& mutable_size() noexcept { return fields_.size; }

    // Getters for the key/value at position i in the node.
    fast_key_type key(int i) const noexcept
    {
        // Key is either returned by a const reference or a value,
        // depending on which makes more sense
        return Params::key(fields_.values[i]);
    }
    reference ref_value(int i) noexcept { return reinterpret_cast<reference>(fields_.values[i]); }
    const_reference ref_value(int i) const noexcept
    {
        return reinterpret_cast<const_reference>(fields_.values[i]);
    }

    // Swap value i in this node with value j in node x.
    void swap_value(int i, btree_node* x, int j) noexcept(is_nothrow_movable)
    {
        detail::swap_helper(fields_.values[i], x->fields_.values[j]);
    }

    // Getters/setter for the child at position i in the node.
    btree_node* child(int i) const noexcept { return fields_.children[i]; }

    void set_child(int i, btree_node* c) noexcept
    {
        fields_.children[i] = c;
        c->fields_.position = i;
        c->fields_.parent = this;
    }

    // Returns the position of the first value whose key is not less than k.
    auto lower_bound(fast_key_type k, const key_compare& comp, std::true_type) const
    {
        return linear_search(k, 0, count(), comp, is_3way_comparer());
    }
    auto lower_bound(fast_key_type k, const key_compare& comp, std::false_type) const
    {
        return binary_search(k, 0, count(), comp, is_3way_comparer());
    }
    auto lower_bound(fast_key_type k, const key_compare& comp) const
    {
        return lower_bound(k, comp, is_linear_search());
    }

    // Returns the position of the first value whose key is greater than k.
    static auto upper_bound_compare(const key_compare& comp, std::true_type) noexcept
    {
        return [&comp](fast_key_type a, fast_key_type b) { return comp(b, a) >= 0; };
    }
    static auto upper_bound_compare(const key_compare& comp, std::false_type) noexcept
    {
        return [&comp](fast_key_type a, fast_key_type b) { return !comp(b, a); };
    }
    static auto upper_bound_compare(const key_compare& comp) noexcept
    {
        return upper_bound_compare(comp, is_3way_comparer());
    }

    search_result<int, false> upper_bound(fast_key_type k, const key_compare& comp,
                                          std::true_type) const
    {
        return linear_search(k, 0, count(), upper_bound_compare(comp), std::false_type());
    }
    search_result<int, false> upper_bound(fast_key_type k, const key_compare& comp,
                                          std::false_type) const
    {
        return binary_search(k, 0, count(), upper_bound_compare(comp), std::false_type());
    }
    search_result<int, false> upper_bound(fast_key_type k, const key_compare& comp) const
    {
        return upper_bound(k, comp, is_linear_search());
    }

    // A helper function to compare to keys using the specified compare
    // functor. This dispatches to the appropriate comparison, depending on
    // whether we have a compare-to functor or not.
    template <typename Compare>
    static bool compare_keys(const Compare& comp, fast_key_type x, fast_key_type y)
    {
        return std::conditional_t<detail::is_3way_comparer<decltype(comp(x, y))>::value,
                                  detail::neg_compare<fast_key_type>,
                                  detail::bool_compare<fast_key_type>>::apply(comp, x, y);
    }

    // Returns the position of the first value whose key is not less than k using
    // linear search performed using plain compare.
    template <typename Compare>
    search_result<int, false> linear_search(fast_key_type k, int s, int e, const Compare& comp,
                                            std::false_type) const
    {
        while (s < e) {
            if (!compare_keys(comp, key(s), k)) {
                break;
            }
            ++s;
        }
        return {s};
    }

    // Returns the position of the first value whose key is not less than k using
    // linear search performed using compare-to.
    search_result<int, true> linear_search(fast_key_type k, int s, int e, const key_compare& comp,
                                           std::true_type) const
    {
        while (s < e) {
            int c = comp(key(s), k);
            if (c == 0) {
                return {s, match_kind::exact};
            } else if (c > 0) {
                break;
            }
            ++s;
        }
        return {s, match_kind::not_exact};
    }

    // Returns the position of the first value whose key is not less than k using
    // binary search performed using plain compare.
    template <typename Compare>
    search_result<int, false> binary_search(fast_key_type k, int s, int e, const Compare& comp,
                                            std::false_type) const
    {
        while (s < e) {
            // Note on midpoint computation: it is not a hidden bug to write mid = (s + e) / 2,
            // because here we are looking only within a single node, which is pretty small.
            // In fact, it will be a compile-time error to select a node size that exceeds the
            // indexation capability of std::uint16_t.
            const int mid = (s + e) / 2;
            if (compare_keys(comp, key(mid), k)) {
                s = mid + 1;
            } else {
                e = mid;
            }
        }
        return {s};
    }

    // Returns the position of the first value whose key is not less than k using
    // binary search performed using compare-to.
    search_result<int, true> binary_search(fast_key_type k, int s, int e, const key_compare& comp,
                                           std::true_type) const
    {
        while (s < e) {
            const int mid = (s + e) / 2;
            const int c = comp(key(mid), k);
            if (c < 0) {
                s = mid + 1;
            } else if (c > 0) {
                e = mid;
            } else {
                // Need to return the first value whose key is not less than k, which
                // requires continuing the binary search. Note that we are guaranteed
                // that the result is an exact match because if "key(mid-1) < k" the
                // call to binary_search_compare_to() will return "mid".
                s = binary_search(k, s, mid, comp, std::true_type()).value;
                return {s, match_kind::exact};
            }
        }
        return {s, match_kind::not_exact};
    }

    // Emplaces the value x at position i, shifting all existing values and
    // children at positions >= i to the right by 1.
    template <typename... Args> void emplace_value(allocator_type& alloc, int i, Args&&... args);

    // Removes the value at position i, shifting all existing values and children
    // at positions > i to the left by 1.
    void remove_value(allocator_type& alloc, int i) noexcept(is_nothrow_operation);

    // Rebalances a node with its right sibling.
    void rebalance_right_to_left(allocator_type& alloc, btree_node* src,
                                 int to_move) noexcept(is_nothrow_operation);
    void rebalance_left_to_right(allocator_type& alloc, btree_node* dst,
                                 int to_move) noexcept(is_nothrow_operation);

    // Splits a node, moving a portion of the node's values to its right sibling.
    void split(allocator_type& alloc, btree_node* dst,
               int insert_position) noexcept(is_nothrow_operation);

    // Merges a node with its right sibling, moving all of the values and the
    // delimiting key in the parent node onto itself.
    void merge(allocator_type& alloc, btree_node* src) noexcept(is_nothrow_operation);

    // Swap the contents of "this" and "dst".
    void swap(allocator_type& alloc, btree_node* dst) noexcept(is_nothrow_operation);

    // Node allocation/deletion routines.
    static btree_node* init_leaf(leaf_fields* f, btree_node* parent, int max_count)
    {
        f->position = 0;
        f->max_count = max_count;
        f->count = 0;
        f->parent = parent;
#if !defined(NDEBUG)
        std::fill_n(reinterpret_cast<char*>(&f->values), max_count * sizeof(value_type), '\0');
#endif
        return reinterpret_cast<btree_node*>(f);
    }
    static btree_node* init_internal(internal_fields* f, btree_node* parent)
    {
        btree_node* n = init_leaf(f, parent, kNodeValues);
        // Set `max_count` to a sentinel value to indicate that this node is internal
        f->max_count = kInternalNodeMaxCount;
#if !defined(NDEBUG)
        std::fill(f->children.begin(), f->children.end(), nullptr);
#endif
        return n;
    }
    static btree_node* init_root(root_fields* f, btree_node* parent)
    {
        btree_node* n = init_internal(f, parent);
        f->rightmost = parent;
        f->size = parent->count();
        return n;
    }
    void destroy(allocator_type& alloc) noexcept(is_nothrow_destructible)
    {
        destroy_values(alloc, 0, count());
    }

private:
    // Trivial setters
    void set_count(int v) noexcept { fields_.count = v; }

    btree_node* assign_child(int i, btree_node* c) noexcept
    {
        assert(c != nullptr);
        fields_.children[i] = c;
        c->fields_.position = i;
        return c;
    }

    void clear_child(int i) noexcept { fields_.children[i] = nullptr; }

    // Swap child i in this node with child j in node x.
    void swap_child(int i, btree_node* x, int j) noexcept
    {
        assert(x != this || i != j);
        assert(!is_leaf() && !x->is_leaf());
        assert(child(i) && x->child(j));
        auto tmp = child(i);
        set_child(i, x->child(j));
        x->set_child(j, tmp);
    }

    // Swap child i in this node with child i in node x.
    void swap_child(int i, btree_node* x) noexcept { swap_child(i, x, i); }

    // Move child i to child j within the same node
    void move_child(int i, int j) noexcept
    {
        assert(!is_leaf() && i != j);
        assign_child(j, child(i));
        clear_child(i);
    }

    // Move child i in this node to child j in node x
    void move_child(int i, btree_node* x, int j) noexcept
    {
        assert(x != this || i != j);
        assert(!is_leaf() && !x->is_leaf());
        x->set_child(j, child(i));
        clear_child(i);
    }

    // Move child i in this node to child i in node x.
    void move_child(int i, btree_node* x) noexcept { move_child(i, x, i); }

private:
    using mutable_iterator = mutable_value_type*;
    using allocator_traits = std::allocator_traits<allocator_type>;

    template <typename... Args>
    static void construct_value(allocator_type& alloc, mutable_value_type* v, Args&&... args)
    {
        allocator_traits::construct(alloc, v, std::forward<Args>(args)...);
    }

    static void destroy_value(allocator_type& alloc,
                              mutable_value_type* v) noexcept(is_nothrow_destructible)
    {
        allocator_traits::destroy(alloc, v);
    }

    mutable_iterator miter(int n) noexcept { return fields_.values + n; };

    // Trivially movable types can just be moved via std::move
    inline static void move_values(allocator_type&, mutable_iterator first, mutable_iterator last,
                                   mutable_iterator out, std::true_type) noexcept
    {
        std::move(first, last, out);
    }

    // Non-trivially movable types need to be moved via move c-tor
    inline static void move_values(allocator_type& alloc, mutable_iterator first,
                                   mutable_iterator last, mutable_iterator out,
                                   std::false_type) noexcept(is_nothrow_movable)
    {
        while (first != last)
            construct_value(alloc, out++, std::move(*first++));
    }

    inline static void move_values(allocator_type& alloc, mutable_iterator first,
                                   mutable_iterator last,
                                   mutable_iterator out) noexcept(is_nothrow_movable)
    {
        move_values(alloc, first, last, out,
                    std::is_trivially_move_constructible<mutable_value_type>());
    }

private:
    template <typename... Args> void construct_value(allocator_type& alloc, int i, Args&&... args)
    {
        assert(i >= 0 && i < max_count());
        construct_value(alloc, &fields_.values[i], std::forward<Args>(args)...);
    }

    void destroy_value(allocator_type& alloc, int i) noexcept(is_nothrow_destructible)
    {
        assert(i >= 0 && i < max_count());
        destroy_value(alloc, &fields_.values[i]);
    }

    // Destroy values in range [s, e)
    void destroy_values(allocator_type& alloc, int s, int e) noexcept(is_nothrow_destructible)
    {
        assert(s >= 0 && s <= e && e <= max_count());
        for (; s < e; ++s)
            destroy_value(alloc, &fields_.values[s]);
    }

    // Swap value i in this node with value i in node x
    void swap_value(int i, btree_node* x) noexcept(is_nothrow_movable)
    {
        assert(x != this);
        detail::swap_helper(fields_.values[i], x->fields_.values[i]);
    }

    // Swap value i with value j
    void swap_value(int i, int j) noexcept(is_nothrow_movable)
    {
        assert(i != j);
        detail::swap_helper(fields_.values[i], fields_.values[j]);
    }

    // Move value i in this node to value j in node x
    void move_value(allocator_type& alloc, int i, btree_node* x, int j) noexcept(is_nothrow_movable)
    {
        assert(x != this || i != j);
        x->construct_value(alloc, j, std::move(fields_.values[i]));
        destroy_value(alloc, i);
    }

    // Move value i in this node to value i in node x
    void move_value(allocator_type& alloc, int i, btree_node* x) noexcept(is_nothrow_movable)
    {
        move_value(alloc, i, x, i);
    }

    // Move value i to value j in the same node
    void move_value(allocator_type& alloc, int i, int j) noexcept(is_nothrow_movable)
    {
        move_value(alloc, i, this, j);
    }

private:
    root_fields fields_;

private:
    btree_node(const btree_node&) = delete;
    btree_node& operator=(const btree_node&) = delete;
};

template <typename Node> struct btree_iterator {
    using key_type = typename Node::key_type;
    using fast_key_type = typename Node::fast_key_type;

    using value_type = typename Node::value_type;
    using size_type = typename Node::size_type;
    using difference_type = typename Node::difference_type;

    static constexpr bool is_const = std::is_const<Node>::value;

    using pointer =
        std::conditional_t<is_const, typename Node::const_pointer, typename Node::pointer>;
    using reference =
        std::conditional_t<is_const, typename Node::const_reference, typename Node::reference>;

    using const_pointer = typename Node::const_pointer;
    using const_reference = typename Node::const_reference;

    using iterator_category = std::bidirectional_iterator_tag;

    constexpr btree_iterator() noexcept
        : node(nullptr)
        , position(-1)
    {
    }

    constexpr btree_iterator(Node* n, int p) noexcept
        : node(n)
        , position(p)
    {
    }

    // Default copy c-tor is fine
    btree_iterator(const btree_iterator& x) = default;

    // Const-converting c-tor
    template <typename Other>
    btree_iterator(btree_iterator<Other> rhs)
        : node(const_cast<Node*>(rhs.node))
        , position(rhs.position)
    {
    }

    // Increment/decrement the iterator.
    void increment()
    {
        if (node->is_leaf() && ++position < node->count()) {
            return;
        }
        increment_slow();
    }
    void increment_by(int count);
    void increment_slow();

    void decrement()
    {
        if (node->is_leaf() && --position >= 0) {
            return;
        }
        decrement_slow();
    }
    void decrement_slow();

    // Accessors for the key/value the iterator is pointing at.
    fast_key_type key() const noexcept { return node->key(position); }
    pointer operator->() const noexcept { return &node->ref_value(position); }
    reference operator*() const noexcept { return node->ref_value(position); }

    btree_iterator& operator++()
    {
        increment();
        return *this;
    }
    btree_iterator& operator--()
    {
        decrement();
        return *this;
    }
    btree_iterator operator++(int)
    {
        btree_iterator tmp = *this;
        ++*this;
        return tmp;
    }
    btree_iterator operator--(int)
    {
        btree_iterator tmp = *this;
        --*this;
        return tmp;
    }

    // The node in the tree the iterator is pointing at.
    Node* node;
    // The position within the node of the tree the iterator is pointing at.
    int position;
};

// Enable comparisons between differently cv-qualified nodes
template <typename U, typename V,
          typename = typename std::is_same<std::remove_cv_t<U>, std::remove_cv_t<V>>::type>
inline bool operator==(const btree_iterator<U>& lhs, const btree_iterator<V>& rhs) noexcept
{
    return lhs.node == rhs.node && lhs.position == rhs.position;
}
template <typename U, typename V>
inline bool operator!=(const btree_iterator<U>& lhs, const btree_iterator<V>& rhs) noexcept
{
    return !(lhs == rhs);
}

template <typename Params> class btree : public Params::key_compare
{
    typedef btree<Params> self_type;
    typedef btree_node<Params> node_type;
    typedef typename node_type::base_fields base_fields;
    typedef typename node_type::leaf_fields leaf_fields;
    typedef typename node_type::internal_fields internal_fields;
    typedef typename node_type::root_fields root_fields;

    enum {
        kNodeValues = node_type::kNodeValues,
        kMinNodeValues = kNodeValues / 2,
    };

    // A helper class to get the empty base class optimization for 0-size
    // allocators. Base is allocator_type.
    // (e.g. empty_base_handle<allocator_type, node_type*>). If Base is
    // 0-size, the compiler doesn't have to reserve any space for it and
    // sizeof(empty_base_handle) will simply be sizeof(Data). Google [empty base
    // class optimization] for more details.
    template <typename Base, typename Data> struct empty_base_handle : public Base {
        empty_base_handle(const Base& b, const Data& d)
            : Base(b)
            , data(d)
        {
        }
        Data data;
    };

    struct node_stats {
        constexpr node_stats(ssize_t l, ssize_t i) noexcept
            : leaf_nodes(l)
            , internal_nodes(i)
        {
        }

        node_stats& operator+=(const node_stats& x) noexcept
        {
            leaf_nodes += x.leaf_nodes;
            internal_nodes += x.internal_nodes;
            return *this;
        }

        ssize_t leaf_nodes;
        ssize_t internal_nodes;
    };

public:
    typedef Params params_type;
    typedef typename Params::key_type key_type;
    typedef typename Params::data_type data_type;
    typedef typename Params::mapped_type mapped_type;
    typedef typename Params::value_type value_type;
    typedef typename Params::key_compare key_compare;
    typedef typename Params::pointer pointer;
    typedef typename Params::const_pointer const_pointer;
    typedef typename Params::reference reference;
    typedef typename Params::const_reference const_reference;
    typedef typename Params::size_type size_type;
    typedef typename Params::difference_type difference_type;

    typedef btree_iterator<node_type> iterator;
    typedef btree_iterator<std::add_const_t<node_type>> const_iterator;
    typedef std::reverse_iterator<const_iterator> const_reverse_iterator;
    typedef std::reverse_iterator<iterator> reverse_iterator;

    typedef typename Params::allocator_type allocator_type;

    using fast_key_type = typename node_type::fast_key_type;

public:
    // Default constructor.
    btree(const key_compare& comp, const allocator_type& alloc);

    // Copy constructor.
    btree(const self_type& x);

    // Destructor.
    ~btree() { clear(); }

    // Allocator routines.
    allocator_type get_allocator() const noexcept
    {
        return *static_cast<const allocator_type*>(&root_);
    }

    // Iterator routines.
    iterator begin() { return iterator(leftmost(), 0); }
    const_iterator begin() const { return const_iterator(leftmost(), 0); }
    iterator end() { return iterator(rightmost(), rightmost() ? rightmost()->count() : 0); }
    const_iterator end() const
    {
        return const_iterator(rightmost(), rightmost() ? rightmost()->count() : 0);
    }
    reverse_iterator rbegin() { return reverse_iterator(end()); }
    const_reverse_iterator rbegin() const { return const_reverse_iterator(end()); }
    reverse_iterator rend() { return reverse_iterator(begin()); }
    const_reverse_iterator rend() const { return const_reverse_iterator(begin()); }

    // Finds the first element whose key is not less than key.
    iterator lower_bound(fast_key_type key)
    {
        return internal_end(internal_lower_bound(key, iterator(root(), 0)));
    }
    const_iterator lower_bound(fast_key_type key) const
    {
        return internal_end(internal_lower_bound(key, const_iterator(root(), 0)));
    }

    // Finds the first element whose key is greater than key.
    iterator upper_bound(fast_key_type key)
    {
        return internal_end(internal_upper_bound(key, iterator(root(), 0)));
    }
    const_iterator upper_bound(fast_key_type key) const
    {
        return internal_end(internal_upper_bound(key, const_iterator(root(), 0)));
    }

    // Finds the range of values which compare equal to key. The first member of
    // the returned pair is equal to lower_bound(key). The second member pair of
    // the pair is equal to upper_bound(key).
    std::pair<iterator, iterator> equal_range(fast_key_type key)
    {
        return std::make_pair(lower_bound(key), upper_bound(key));
    }
    std::pair<const_iterator, const_iterator> equal_range(fast_key_type key) const
    {
        return std::make_pair(lower_bound(key), upper_bound(key));
    }

    // Inserts a value into the btree only if it does not already exist. The
    // boolean return value indicates whether insertion succeeded or failed.
    template <typename... Args>
    std::pair<iterator, bool> emplace_unique_key_args(fast_key_type key, Args&&... args);

    template <typename... Args>
    iterator emplace_hint_unique_key_args(iterator hint, fast_key_type key, Args&&... args);

    std::pair<iterator, bool> insert_unique(const value_type& v)
    {
        return emplace_unique_key_args(params_type::key(v), v);
    }
    std::pair<iterator, bool> insert_unique(value_type&& v)
    {
        return emplace_unique_key_args(params_type::key(v), std::move(v));
    }
    template <typename P> std::pair<iterator, bool> insert_unique(P&& v)
    {
        return emplace_unique_key_args(params_type::key(v), std::move(v));
    }

    // Insert with hint. Check to see if the value should be placed immediately
    // before position in the tree. If it does, then the insertion will take
    // amortized constant time. If not, the insertion will take amortized
    // logarithmic time as if a call to insert_unique(v) were made.
    iterator insert_unique(iterator hint, const value_type& v)
    {
        return emplace_hint_unique_key_args(hint, params_type::key(v), v);
    }
    iterator insert_unique(iterator hint, value_type&& v)
    {
        return emplace_hint_unique_key_args(hint, params_type::key(v), std::move(v));
    }
    template <typename P> iterator insert_unique(iterator hint, P&& v)
    {
        return emplace_hint_unique_key_args(hint, params_type::key(v), std::move(v));
    }

    // Insert a range of values into the btree.
    template <typename InputIterator> void insert_unique(InputIterator b, InputIterator e);

    // Inserts a value into the btree.
    template <typename... Args> iterator emplace_key_args(fast_key_type key, Args&&... args);

    // Inserts a value into the btree.
    iterator insert_multi(const value_type& v) { return emplace_key_args(params_type::key(v), v); }
    iterator insert_multi(value_type&& v)
    {
        return emplace_key_args(params_type::key(v), std::move(v));
    }
    template <typename P> iterator insert_multi(P&& v)
    {
        return emplace_key_args(params_type::key(v), std::move(v));
    }

    // Insert with hint. Check to see if the value should be placed immediately
    // before position in the tree. If it does, then the insertion will take
    // amortized constant time. If not, the insertion will take amortized
    // logarithmic time as if a call to insert_multi(v) were made.
    iterator insert_multi(iterator position, const value_type& v);

    // Insert a range of values into the btree.
    template <typename InputIterator> void insert_multi(InputIterator b, InputIterator e);

    void assign(const self_type& x);

    // Erase the specified iterator from the btree. The iterator must be valid
    // (i.e. not equal to end()).  Return an iterator pointing to the node after
    // the one that was erased (or end() if none exists).
    iterator erase(iterator iter);

    // Erases range. Returns the number of keys erased.
    size_type erase(iterator begin, iterator end);

    // Erases the specified key from the btree. Returns 1 if an element was
    // erased and 0 otherwise.
    size_type erase_unique(fast_key_type key);

    // Erases all of the entries matching the specified key from the
    // btree. Returns the number of elements erased.
    size_type erase_multi(fast_key_type key);

    // Finds the iterator corresponding to a key or returns end() if the key is
    // not present.
    iterator find_unique(fast_key_type key)
    {
        return internal_end(internal_find_unique(key, iterator(root(), 0)));
    }
    const_iterator find_unique(fast_key_type key) const
    {
        return internal_end(internal_find_unique(key, const_iterator(root(), 0)));
    }
    iterator find_multi(fast_key_type key)
    {
        return internal_end(internal_find_multi(key, iterator(root(), 0)));
    }
    const_iterator find_multi(fast_key_type key) const
    {
        return internal_end(internal_find_multi(key, const_iterator(root(), 0)));
    }

    // Returns a count of the number of times the key appears in the btree.
    size_type count_unique(fast_key_type key) const
    {
        return !!internal_find_unique(key, const_iterator(root(), 0)).node;
    }

    // Returns a count of the number of times the key appears in the btree.
    size_type count_multi(fast_key_type key) const
    {
        return distance(lower_bound(key), upper_bound(key));
    }

    // Clear the btree, deleting all of the values it contains.
    void clear();

    // Swap the contents of *this and x.
    void swap(self_type& x);

    // Assign the contents of x to *this.
    self_type& operator=(const self_type& x)
    {
        assign(x);
        return *this;
    }

    const key_compare& key_comp() const noexcept { return *this; }
    bool compare_keys(fast_key_type x, fast_key_type y) const
    {
        return node_type::compare_keys(key_comp(), x, y);
    }

    // Dump the btree to the specified ostream. Requires that operator<< is
    // defined for Key and Value.
    void dump(std::ostream& os) const
    {
        if (root() != nullptr) {
            internal_dump(os, root(), 0);
        }
    }

    // Verifies the structure of the btree.
    void verify() const;

    // Size routines. Note that empty() is slightly faster than doing size()==0.
    size_type size() const
    {
        if (empty())
            return 0;
        if (root()->is_leaf())
            return root()->count();
        return root()->size();
    }
    size_type max_size() const noexcept { return std::numeric_limits<size_type>::max(); }
    bool empty() const noexcept { return root() == nullptr; }

    // The height of the btree. An empty tree will have height 0.
    size_type height() const noexcept
    {
        size_type h = 0;
        if (root()) {
            // Count the length of the chain from the leftmost node up to the
            // root. We actually count from the root back around to the level below
            // the root, but the calculation is the same because of the circularity
            // of that traversal.
            const node_type* n = root();
            do {
                ++h;
                n = n->parent();
            } while (n != root());
        }
        return h;
    }

    // The number of internal, leaf and total nodes used by the btree.
    size_type leaf_nodes() const { return internal_stats(root()).leaf_nodes; }
    size_type internal_nodes() const { return internal_stats(root()).internal_nodes; }
    size_type nodes() const
    {
        const node_stats stats = internal_stats(root());
        return stats.leaf_nodes + stats.internal_nodes;
    }

    // The total number of bytes used by the btree.
    size_type bytes_used() const
    {
        const node_stats stats = internal_stats(root());
        if (stats.leaf_nodes == 1 && stats.internal_nodes == 0) {
            return sizeof(*this) + sizeof(base_fields) + root()->max_count() * sizeof(value_type);
        } else {
            return sizeof(*this) + sizeof(root_fields) - sizeof(internal_fields) +
                   stats.leaf_nodes * sizeof(leaf_fields) +
                   stats.internal_nodes * sizeof(internal_fields);
        }
    }

    // The average number of bytes used per value stored in the btree.
    static double average_bytes_per_value() noexcept
    {
        // Returns the number of bytes per value on a leaf node that is 75%
        // full. Experimentally, this matches up nicely with the computed number of
        // bytes per value in trees that had their values inserted in random order.
        return sizeof(leaf_fields) / (kNodeValues * 0.75);
    }

    // The fullness of the btree. Computed as the number of elements in the btree
    // divided by the maximum number of elements a tree with the current number
    // of nodes could hold. A value of 1 indicates perfect space
    // utilization. Smaller values indicate space wastage.
    double fullness() const { return double(size()) / (nodes() * kNodeValues); }
    // The overhead of the btree structure in bytes per node. Computed as the
    // total number of bytes used by the btree minus the number of bytes used for
    // storing elements divided by the number of elements.
    double overhead() const
    {
        if (empty()) {
            return 0.0;
        }
        return (bytes_used() - size() * sizeof(value_type)) / double(size());
    }

private:
    using byte_allocator_type = typename allocator_type::template rebind<std::uint8_t>::other;

    using optional_key = std::pair<fast_key_type, bool>;

    // Internal accessor routines.
    node_type* root() noexcept { return root_.data; }
    const node_type* root() const noexcept { return root_.data; }
    node_type** mutable_root() { return &root_.data; }

    // Mutable allocator
    allocator_type& get_allocator() noexcept { return *static_cast<allocator_type*>(&root_); }

    // Mutable key comparator
    key_compare& key_comp() noexcept { return *this; }

    // The rightmost node is stored in the root node.
    node_type* rightmost() { return (!root() || root()->is_leaf()) ? root() : root()->rightmost(); }
    const node_type* rightmost() const
    {
        return (!root() || root()->is_leaf()) ? root() : root()->rightmost();
    }
    node_type** mutable_rightmost() { return root()->mutable_rightmost(); }

    // The leftmost node is stored as the parent of the root node.
    node_type* leftmost() noexcept { return root() ? root()->parent() : nullptr; }
    const node_type* leftmost() const noexcept { return root() ? root()->parent() : nullptr; }

    // The size of the tree is stored in the root node.
    size_type& mutable_size() noexcept { return root()->mutable_size(); }

    template <typename Node> Node* make_node()
    {
        byte_allocator_type byte_alloc(get_allocator());
        return reinterpret_cast<Node*>(byte_alloc.allocate(sizeof(Node)));
    }

    // Node creation/deletion routines.
    node_type* new_internal_node(node_type* parent)
    {
        auto p = make_node<internal_fields>();
        return node_type::init_internal(p, parent);
    }
    node_type* new_internal_root_node()
    {
        auto p = make_node<root_fields>();
        return node_type::init_root(p, root()->parent());
    }
    node_type* new_leaf_node(node_type* parent)
    {
        auto p = make_node<leaf_fields>();
        return node_type::init_leaf(p, parent, kNodeValues);
    }
    node_type* new_leaf_root_node(int max_count)
    {
        byte_allocator_type byte_alloc(get_allocator());
        leaf_fields* p = reinterpret_cast<leaf_fields*>(
            byte_alloc.allocate(sizeof(base_fields) + max_count * sizeof(value_type)));
        return node_type::init_leaf(p, reinterpret_cast<node_type*>(p), max_count);
    }

    void delete_node(node_type* p, std::size_t nbytes)
    {
        byte_allocator_type byte_alloc(get_allocator());
        byte_alloc.deallocate(reinterpret_cast<std::uint8_t*>(p), nbytes);
    }

    void delete_internal_node(node_type* node)
    {
        node->destroy(get_allocator());
        assert(node != root());
        delete_node(node, sizeof(internal_fields));
    }
    void delete_internal_root_node()
    {
        root()->destroy(get_allocator());
        delete_node(root(), sizeof(root_fields));
    }
    void delete_leaf_node(node_type* node)
    {
        node->destroy(get_allocator());
        delete_node(node, sizeof(base_fields) + node->max_count() * sizeof(value_type));
    }

    // Rebalances or splits the node iter points to.
    void rebalance_or_split(iterator* iter);

    // Merges the values of left, right and the delimiting key on their parent
    // onto left, removing the delimiting key and deleting right.
    void merge_nodes(node_type* left, node_type* right);

    // Tries to merge node with its left or right sibling, and failing that,
    // rebalance with its left or right sibling. Returns true if a merge
    // occurred, at which point it is no longer valid to access node. Returns
    // false if no merging took place.
    bool try_merge_or_rebalance(iterator* iter);

    // Tries to shrink the height of the tree by 1.
    void try_shrink();

    iterator internal_end(iterator iter) { return iter.node ? iter : end(); }
    const_iterator internal_end(const_iterator iter) const { return iter.node ? iter : end(); }

    // Emplaces a value into the btree immediately before iter. Requires that
    // key(v) <= iter.key() and std::prev(iter).key() <= key(v).
    template <typename... Args> iterator internal_emplace(iterator iter, Args&&... args);

    iterator internal_insert(iterator iter, const value_type& v)
    {
        return internal_emplace(iter, v);
    }

    // Returns an iterator pointing to the first value >= the value "iter" is
    // pointing at. Note that "iter" might be pointing to an invalid location as
    // iter.position == iter.node->count(). This routine simply moves iter up in
    // the tree to a valid location.
    template <typename IterType> static IterType internal_last(IterType iter);

    // Returns an iterator pointing to the leaf position at which key would reside in the tree.
    // We will return either match_kind::exact (if the key was found in the tree) or
    // match_kind::not_exact (if it wasn't) in the second field of the pair.
    template <typename IterType>
    std::pair<IterType, match_kind> internal_locate(fast_key_type key, IterType iter) const;

    // Internal routine which implements lower_bound().
    template <typename IterType>
    IterType internal_lower_bound(fast_key_type key, IterType iter) const;

    // Internal routine which implements upper_bound().
    template <typename IterType>
    IterType internal_upper_bound(fast_key_type key, IterType iter) const;

    // Internal routine which implements find_unique().
    template <typename IterType>
    IterType internal_find_unique(fast_key_type key, IterType iter) const;

    // Internal routine which implements find_multi().
    template <typename IterType>
    IterType internal_find_multi(fast_key_type key, IterType iter) const;

    // Deletes a node and all of its children.
    void internal_clear(node_type* node);

    // Dumps a node and all of its children to the specified ostream.
    void internal_dump(std::ostream& os, const node_type* node, int level) const;

    // Verifies the tree structure of node.
    int internal_verify(const node_type* node, optional_key lo, optional_key hi) const;

    node_stats internal_stats(const node_type* node) const noexcept
    {
        if (!node) {
            return node_stats(0, 0);
        }
        if (node->is_leaf()) {
            return node_stats(1, 0);
        }
        node_stats res(0, 1);
        for (int i = 0; i <= node->count(); ++i) {
            res += internal_stats(node->child(i));
        }
        return res;
    }

private:
    empty_base_handle<allocator_type, node_type*> root_;
};

////
// btree_node member functions
template <typename P>
template <typename... Args>
inline void btree_node<P>::emplace_value(allocator_type& alloc, int i, Args&&... args)
{
    const int cnt = count();

    assert(i <= cnt && cnt < max_count());

    // Construct the value at the end, allowing the insertion to fail here first, before
    // doing anything else with the tree. It is pretty much as close to the strong
    // exception safety as we can get efficiently.
    construct_value(alloc, cnt, std::forward<Args>(args)...);

    const int dst_cnt = cnt + 1;

    // Rotate the newly constructed value to the correct position
    std::rotate(miter(i), miter(cnt), miter(dst_cnt));

    // Also move the children data for internal nodes
    if (!is_leaf()) {
        for (int j = cnt; j > i; --j) {
            assign_child(j + 1, child(j));
        }
        clear_child(i + 1);
    }

    // Increase the number of items
    set_count(dst_cnt);
}

template <typename P>
inline void btree_node<P>::remove_value(allocator_type& alloc, int i) noexcept(is_nothrow_operation)
{
    const int cnt = count();

    assert(cnt > 0 && i < cnt);

    const int right = i + 1;
    const int dst_cnt = cnt - 1;

    // Move values to the right
    std::move_backward(miter(right), miter(cnt), miter(dst_cnt));

    if (!is_leaf()) {
        // assert(child(right)->count() == 0);
        for (int j = right; j < cnt; ++j) {
            assign_child(j, child(j + 1));
        }
        clear_child(cnt);
    }

    // Decrease the number of items
    set_count(dst_cnt);

    // Finally, destroy the value
    destroy_value(alloc, dst_cnt);
}

template <typename P>
void btree_node<P>::rebalance_right_to_left(allocator_type& alloc, btree_node* src,
                                            int to_move) noexcept(is_nothrow_operation)
{
    const int cnt = count();
    const int src_cnt = src->count();

    assert(parent() == src->parent());
    assert(position() + 1 == src->position());
    assert(src_cnt >= cnt);
    assert(to_move >= 1 && to_move <= src_cnt);

    // Move the delimiting value to the left node.
    parent()->move_value(alloc, position(), this, cnt);

    // Move the new delimiting value from the right node.
    src->move_value(alloc, to_move - 1, parent(), position());

    if (is_leaf()) {
        // Move the values from the right to the left node
        for (int i = 1; i < to_move; ++i) {
            src->move_value(alloc, i - 1, this, cnt + i);
        }
        // Shift the values in the right node to their correct position
        for (int i = to_move; i < src_cnt; ++i) {
            src->move_value(alloc, i, i - to_move);
        }
    } else {
        // Move the values and child pointers from the right to the left node
        src->move_child(0, this, 1 + cnt);
        for (int i = 1; i < to_move; ++i) {
            src->move_value(alloc, i - 1, this, cnt + i);
            src->move_child(i, this, 1 + cnt + i);
        }
        // Shift the values and child pointers in the right node to their correct position
        for (int i = to_move; i < src_cnt; ++i) {
            src->move_value(alloc, i, i - to_move);
            src->move_child(i, i - to_move);
        }
        src->move_child(src_cnt, src_cnt - to_move);
    }

    // Fixup the counts on the src and dst nodes.
    set_count(cnt + to_move);
    src->set_count(src_cnt - to_move);
}

template <typename P>
void btree_node<P>::rebalance_left_to_right(allocator_type& alloc, btree_node* dst,
                                            int to_move) noexcept(is_nothrow_operation)
{
    const int cnt = count();
    const int dst_cnt = dst->count();

    assert(parent() == dst->parent());
    assert(position() + 1 == dst->position());
    assert(cnt >= dst_cnt);
    assert(to_move >= 1 && to_move <= cnt);

    // Make room in the right node for the new values
    for (int i = dst_cnt - 1; i >= 0; --i) {
        dst->move_value(alloc, i, i + to_move);
    }

    // Move the delimiting value to the right node
    parent()->move_value(alloc, position(), dst, to_move - 1);

    // Move the new delimiting value from the left node
    move_value(alloc, cnt - to_move, parent(), position());

    if (is_leaf()) {
        // Move the values from the left to the right node
        for (int i = 1; i < to_move; ++i) {
            move_value(alloc, cnt - to_move + i, dst, i - 1);
        }
    } else {
        // Move the values and child pointers from the left to the right node
        for (int i = dst_cnt; i >= 0; --i) {
            dst->move_child(i, i + to_move);
        }
        // Move the values and child pointers from the left to the right node
        for (int i = 1; i < to_move; ++i) {
            move_value(alloc, cnt - to_move + i, dst, i - 1);
            move_child(cnt - to_move + i, dst, i - 1);
        }
        move_child(cnt, dst, to_move - 1);
    }

    // Fixup the counts on the src and dst nodes.
    set_count(cnt - to_move);
    dst->set_count(dst_cnt + to_move);
}

template <typename P>
void btree_node<P>::split(allocator_type& alloc, btree_node* dst,
                          int insert_position) noexcept(is_nothrow_operation)
{
    const int cnt = count();
    int dst_cnt = dst->count();

    assert(dst_cnt == 0);

    // We bias the split based on the position being inserted. If we're
    // inserting at the beginning of the left node then bias the split to put
    // more values on the right node. If we're inserting at the end of the
    // right node then bias the split to put more values on the left node.
    if (insert_position == 0) {
        dst_cnt = cnt - 1;
    } else if (insert_position != max_count()) {
        dst_cnt = cnt / 2;
    }
    assert(cnt > dst_cnt);

    const int mov_cnt = cnt - dst_cnt;
    const int src_cnt = mov_cnt - 1;

    // The split key is the largest value in the left sibling, move it up
    parent()->emplace_value(alloc, position(), std::move(*miter(src_cnt)));

    // Move values from the left sibling to the right sibling
    move_values(alloc, miter(mov_cnt), miter(cnt), dst->miter(0));

    if (!is_leaf()) {
        // Move child pointers from the left sibling to the right sibling
        for (int i = 0; i <= dst_cnt; ++i) {
            move_child(mov_cnt + i, dst, i);
        }
    }

    // Update the counts in split nodes
    set_count(src_cnt);
    dst->set_count(dst_cnt);
    parent()->set_child(position() + 1, dst);

    // Finally, destroy the moved-from values
    destroy_values(alloc, src_cnt, cnt);
}

template <typename P>
void btree_node<P>::merge(allocator_type& alloc, btree_node* src) noexcept(is_nothrow_operation)
{
    assert(parent() == src->parent());
    assert(position() + 1 == src->position());

    int cnt = count();

    // Move the delimiting value to the left node, and at the same token
    // register that we have more values there
    emplace_value(alloc, cnt++, std::move(*parent()->miter(position())));
    parent()->remove_value(alloc, position());

    const int src_cnt = src->count();

    // Move the values from the right to the left node
    move_values(alloc, src->miter(0), src->miter(src_cnt), miter(cnt));

    if (!is_leaf()) {
        // Move the child pointers from the right to the left node
        for (int i = 0; i <= src_cnt; ++i) {
            src->move_child(i, this, cnt + i);
        }
    }

    // Fixup the counts on the src and dst nodes
    set_count(cnt + src_cnt);
    src->set_count(0);

    // Finally, destroy the values
    src->destroy_values(alloc, 0, src_cnt);
}

template <typename P>
void btree_node<P>::swap(allocator_type& alloc, btree_node* dst) noexcept(is_nothrow_operation)
{
    const int cnt = count();
    const int dst_cnt = dst->count();

    const int min = std::min(cnt, dst_cnt);

    assert(is_leaf() == dst->is_leaf());

    if (is_leaf()) {
        // Swap the values
        for (int i = 0; i < min; ++i) {
            swap_value(i, dst);
        }
        for (int i = min; i < dst_cnt; ++i) {
            dst->move_value(alloc, i, this);
        }
        for (int i = min; i < cnt; ++i) {
            move_value(alloc, i, dst);
        }
    } else {
        // Swap the values and child pointers
        for (int i = 0; i < min; ++i) {
            swap_value(i, dst);
            swap_child(i, dst);
        }
        swap_child(min, dst);
        for (int i = min; i < dst_cnt; ++i) {
            dst->move_value(alloc, i, this);
            dst->move_child(i + 1, this);
        }
        for (int i = min; i < cnt; ++i) {
            move_value(alloc, i, dst);
            move_child(i + 1, dst);
        }
    }

    // Swap the counts
    std::swap(fields_.count, dst->fields_.count);
}

////
// btree_iterator member functions
template <typename N> inline void btree_iterator<N>::increment_slow()
{
    if (node->is_leaf()) {
        assert(position >= node->count());
        btree_iterator save(*this);
        while (position == node->count() && !node->is_root()) {
            assert(node->parent()->child(node->position()) == node);
            position = node->position();
            node = node->parent();
        }
        if (position == node->count()) {
            *this = save;
        }
    } else {
        assert(position < node->count());
        node = node->child(position + 1);
        while (!node->is_leaf()) {
            node = node->child(0);
        }
        position = 0;
    }
}

template <typename N> inline void btree_iterator<N>::increment_by(int count)
{
    while (count > 0) {
        if (node->is_leaf()) {
            int rest = node->count() - position;
            position += std::min(rest, count);
            count = count - rest;
            if (position < node->count()) {
                return;
            }
        } else {
            --count;
        }
        increment_slow();
    }
}

template <typename N> inline void btree_iterator<N>::decrement_slow()
{
    if (node->is_leaf()) {
        assert(position <= -1);
        btree_iterator save(*this);
        while (position < 0 && !node->is_root()) {
            assert(node->parent()->child(node->position()) == node);
            position = node->position() - 1;
            node = node->parent();
        }
        if (position < 0) {
            *this = save;
        }
    } else {
        assert(position >= 0);
        node = node->child(position);
        while (!node->is_leaf()) {
            node = node->child(node->count());
        }
        position = node->count() - 1;
    }
}

////
// btree member functions
template <typename P>
inline btree<P>::btree(const key_compare& comp, const allocator_type& alloc)
    : key_compare(comp)
    , root_(alloc, nullptr)
{
}

template <typename P>
inline btree<P>::btree(const self_type& x)
    : key_compare(x.key_comp())
    , root_(x.get_allocator(), nullptr)
{
    assign(x);
}

template <typename P>
template <typename... Args>
inline std::pair<typename btree<P>::iterator, bool> btree<P>::emplace_unique_key_args(
    fast_key_type key, Args&&... args)
{
    if (empty()) {
        *mutable_root() = new_leaf_root_node(1);
    }

    auto res = internal_locate(key, iterator(root(), 0));
    if (res.second == match_kind::exact) {
        // The key already exists in the tree, do nothing.
        return std::make_pair(res.first, false);
    }

    return std::make_pair(internal_emplace(res.first, std::forward<Args>(args)...), true);
}

template <typename P>
template <typename... Args>
inline typename btree<P>::iterator btree<P>::emplace_hint_unique_key_args(iterator hint,
                                                                          fast_key_type key,
                                                                          Args&&... args)
{
    if (!empty()) {
        if (hint == end() || compare_keys(key, hint.key())) {
            if (hint == begin() || compare_keys(std::prev(hint).key(), key)) {
                // prev.key() < key < hint.key()
                return internal_emplace(hint, std::forward<Args>(args)...);
            }
        } else if (compare_keys(hint.key(), key)) {
            iterator next = std::next(hint);
            if (next == end() || compare_keys(key, next.key())) {
                // hint.key() < key < next.key()
                return internal_emplace(next, std::forward<Args>(args)...);
            }
        } else {
            // hint.key() == key
            return hint;
        }
    }
    return emplace_unique_key_args(key, std::forward<Args>(args)...).first;
}

template <typename P>
template <typename InputIterator>
inline void btree<P>::insert_unique(InputIterator b, InputIterator e)
{
    for (; b != e; ++b) {
        insert_unique(end(), *b);
    }
}

template <typename P>
template <typename... Args>
inline typename btree<P>::iterator btree<P>::emplace_key_args(fast_key_type key, Args&&... args)
{
    if (empty()) {
        *mutable_root() = new_leaf_root_node(1);
    }

    iterator iter = internal_upper_bound(key, iterator(root(), 0));
    if (!iter.node) {
        iter = end();
    }
    return internal_emplace(iter, std::forward<Args>(args)...);
}

template <typename P>
inline typename btree<P>::iterator btree<P>::insert_multi(iterator position, const value_type& v)
{
    if (!empty()) {
        fast_key_type key = params_type::key(v);
        if (position == end() || !compare_keys(position.key(), key)) {
            if (position == begin() || !compare_keys(key, std::prev(position).key())) {
                // prev.key() <= key <= position.key()
                return internal_insert(position, v);
            }
        } else {
            iterator next = position;
            ++next;
            if (next == end() || !compare_keys(next.key(), key)) {
                // position.key() < key <= next.key()
                return internal_insert(next, v);
            }
        }
    }
    return insert_multi(v);
}

template <typename P>
template <typename InputIterator>
inline void btree<P>::insert_multi(InputIterator b, InputIterator e)
{
    for (; b != e; ++b) {
        insert_multi(end(), *b);
    }
}

template <typename P> inline void btree<P>::assign(const self_type& x)
{
    if (&x != this) {
        // Don't copy onto ourselves.
        clear();

        key_comp() = x.key_comp();
        get_allocator() = x.get_allocator();

        // Assignment can avoid key comparisons because we know the order of the
        // values is the same order we'll store them in.
        for (const_iterator iter = x.begin(); iter != x.end(); ++iter) {
            if (empty()) {
                insert_multi(*iter);
            } else {
                // If the btree is not empty, we can just insert the new value at the end
                // of the tree!
                internal_insert(end(), *iter);
            }
        }
    }
}

template <typename P> inline typename btree<P>::iterator btree<P>::erase(iterator iter)
{
    bool internal_delete = false;
    if (!iter.node->is_leaf()) {
        // Deletion of a value on an internal node. Swap the key with the largest
        // value of our left child. This is easy, we just decrement iter.
        iterator tmp_iter(iter--);
        assert(iter.node->is_leaf());
        assert(!compare_keys(tmp_iter.key(), iter.key()));
        iter.node->swap_value(iter.position, tmp_iter.node, tmp_iter.position);
        internal_delete = true;
        --mutable_size();
    } else if (!root()->is_leaf()) {
        --mutable_size();
    }

    // Delete the key from the leaf.
    iter.node->remove_value(get_allocator(), iter.position);

    // We want to return the next value after the one we just erased. If we
    // erased from an internal node (internal_delete == true), then the next
    // value is ++(++iter). If we erased from a leaf node (internal_delete ==
    // false) then the next value is ++iter. Note that ++iter may point to an
    // internal node and the value in the internal node may move to a leaf node
    // (iter.node) when rebalancing is performed at the leaf level.

    // Merge/rebalance as we walk back up the tree.
    iterator res(iter);
    for (;;) {
        if (iter.node == root()) {
            try_shrink();
            if (empty()) {
                return end();
            }
            break;
        }
        if (iter.node->count() >= kMinNodeValues) {
            break;
        }
        bool merged = try_merge_or_rebalance(&iter);
        if (iter.node->is_leaf()) {
            res = iter;
        }
        if (!merged) {
            break;
        }
        iter.node = iter.node->parent();
    }

    // Adjust our return value. If we're pointing at the end of a node, advance
    // the iterator.
    if (res.position == res.node->count()) {
        res.position = res.node->count() - 1;
        ++res;
    }
    // If we erased from an internal node, advance the iterator.
    if (internal_delete) {
        ++res;
    }
    return res;
}

template <typename P>
inline typename btree<P>::size_type btree<P>::erase(iterator begin, iterator end)
{
    const size_type count = distance(begin, end);
    for (size_type i = 0; i < count; ++i) {
        begin = erase(begin);
    }
    return count;
}

template <typename P> inline typename btree<P>::size_type btree<P>::erase_unique(fast_key_type key)
{
    iterator iter = internal_find_unique(key, iterator(root(), 0));
    if (!iter.node) {
        // The key doesn't exist in the tree, return nothing done.
        return 0;
    }
    erase(iter);
    return 1;
}

template <typename P> inline typename btree<P>::size_type btree<P>::erase_multi(fast_key_type key)
{
    iterator begin = internal_lower_bound(key, iterator(root(), 0));
    if (!begin.node) {
        // The key doesn't exist in the tree, return nothing done.
        return 0;
    }
    iterator end = internal_end(internal_upper_bound(key, iterator(root(), 0)));
    // Delete all of the keys between begin and upper_bound(key).
    return erase(begin, end);
}

template <typename P> inline void btree<P>::clear()
{
    if (root() != nullptr) {
        internal_clear(root());
    }
    *mutable_root() = nullptr;
}

template <typename P> inline void btree<P>::swap(self_type& x)
{
    std::swap(key_comp(), x.key_comp());
    std::swap(root_, x.root_);
}

template <typename P> inline void btree<P>::verify() const
{
    if (root() != nullptr) {
        assert(size() == internal_verify(root(), nullptr, nullptr));
        assert(leftmost() == (++const_iterator(root(), -1)).node);
        assert(rightmost() == (--const_iterator(root(), root()->count())).node);
        assert(leftmost()->is_leaf());
        assert(rightmost()->is_leaf());
    } else {
        assert(size() == 0);
        assert(leftmost() == nullptr);
        assert(rightmost() == nullptr);
    }
}

template <typename P> inline void btree<P>::rebalance_or_split(iterator* iter)
{
    node_type*& node = iter->node;
    int& insert_position = iter->position;
    assert(node->count() == node->max_count());

    allocator_type& alloc = get_allocator();

    // First try to make room on the node by rebalancing.
    node_type* parent = node->parent();
    if (node != root()) {
        if (node->position() > 0) {
            // Try rebalancing with our left sibling.
            node_type* left = parent->child(node->position() - 1);
            if (left->count() < left->max_count()) {
                // We bias rebalancing based on the position being inserted. If we're
                // inserting at the end of the right node then we bias rebalancing to
                // fill up the left node.
                int to_move = (left->max_count() - left->count()) /
                              (1 + (insert_position < left->max_count()));
                to_move = std::max(1, to_move);

                if (((insert_position - to_move) >= 0) ||
                    ((left->count() + to_move) < left->max_count())) {
                    left->rebalance_right_to_left(alloc, node, to_move);

                    assert(node->max_count() - node->count() == to_move);
                    insert_position = insert_position - to_move;
                    if (insert_position < 0) {
                        insert_position = insert_position + left->count() + 1;
                        node = left;
                    }

                    assert(node->count() < node->max_count());
                    return;
                }
            }
        }

        if (node->position() < parent->count()) {
            // Try rebalancing with our right sibling.
            node_type* right = parent->child(node->position() + 1);
            if (right->count() < right->max_count()) {
                // We bias rebalancing based on the position being inserted. If we're
                // inserting at the beginning of the left node then we bias rebalancing
                // to fill up the right node.
                int to_move = (right->max_count() - right->count()) / (1 + (insert_position > 0));
                to_move = std::max(1, to_move);

                if ((insert_position <= (node->count() - to_move)) ||
                    ((right->count() + to_move) < right->max_count())) {
                    node->rebalance_left_to_right(alloc, right, to_move);

                    if (insert_position > node->count()) {
                        insert_position = insert_position - node->count() - 1;
                        node = right;
                    }

                    assert(node->count() < node->max_count());
                    return;
                }
            }
        }

        // Rebalancing failed, make sure there is room on the parent node for a new
        // value.
        if (parent->count() == parent->max_count()) {
            iterator parent_iter(node->parent(), node->position());
            rebalance_or_split(&parent_iter);
        }
    } else {
        // Rebalancing not possible because this is the root node.
        if (root()->is_leaf()) {
            // The root node is currently a leaf node: create a new root node and set
            // the current root node as the child of the new root.
            parent = new_internal_root_node();
            parent->set_child(0, root());
            *mutable_root() = parent;
            assert(*mutable_rightmost() == parent->child(0));
        } else {
            // The root node is an internal node. We do not want to create a new root
            // node because the root node is special and holds the size of the tree
            // and a pointer to the rightmost node. So we create a new internal node
            // and move all of the items on the current root into the new node.
            parent = new_internal_node(parent);
            parent->set_child(0, parent);
            parent->swap(alloc, root());
            node = parent;
        }
    }

    // Split the node.
    node_type* split_node;
    if (node->is_leaf()) {
        split_node = new_leaf_node(parent);
        node->split(alloc, split_node, insert_position);
        if (rightmost() == node) {
            *mutable_rightmost() = split_node;
        }
    } else {
        split_node = new_internal_node(parent);
        node->split(alloc, split_node, insert_position);
    }

    if (insert_position > node->count()) {
        insert_position = insert_position - node->count() - 1;
        node = split_node;
    }
}

template <typename P> inline void btree<P>::merge_nodes(node_type* left, node_type* right)
{
    left->merge(get_allocator(), right);
    if (right->is_leaf()) {
        if (rightmost() == right) {
            *mutable_rightmost() = left;
        }
        delete_leaf_node(right);
    } else {
        delete_internal_node(right);
    }
}

template <typename P> inline bool btree<P>::try_merge_or_rebalance(iterator* iter)
{
    node_type* parent = iter->node->parent();

    allocator_type& alloc = get_allocator();

    if (iter->node->position() > 0) {
        // Try merging with our left sibling.
        node_type* left = parent->child(iter->node->position() - 1);
        if ((1 + left->count() + iter->node->count()) <= left->max_count()) {
            iter->position += 1 + left->count();
            merge_nodes(left, iter->node);
            iter->node = left;
            return true;
        }
    }
    if (iter->node->position() < parent->count()) {
        // Try merging with our right sibling.
        node_type* right = parent->child(iter->node->position() + 1);
        if ((1 + iter->node->count() + right->count()) <= right->max_count()) {
            merge_nodes(iter->node, right);
            return true;
        }
        // Try rebalancing with our right sibling. We don't perform rebalancing if
        // we deleted the first element from iter->node and the node is not
        // empty. This is a small optimization for the common pattern of deleting
        // from the front of the tree.
        if ((right->count() > kMinNodeValues) &&
            ((iter->node->count() == 0) || (iter->position > 0))) {
            int to_move = (right->count() - iter->node->count()) / 2;
            to_move = std::min(to_move, right->count() - 1);
            iter->node->rebalance_right_to_left(alloc, right, to_move);
            return false;
        }
    }
    if (iter->node->position() > 0) {
        // Try rebalancing with our left sibling. We don't perform rebalancing if
        // we deleted the last element from iter->node and the node is not
        // empty. This is a small optimization for the common pattern of deleting
        // from the back of the tree.
        node_type* left = parent->child(iter->node->position() - 1);
        if ((left->count() > kMinNodeValues) &&
            ((iter->node->count() == 0) || (iter->position < iter->node->count()))) {
            int to_move = (left->count() - iter->node->count()) / 2;
            to_move = std::min(to_move, left->count() - 1);
            left->rebalance_left_to_right(alloc, iter->node, to_move);
            iter->position += to_move;
            return false;
        }
    }
    return false;
}

template <typename P> inline void btree<P>::try_shrink()
{
    if (root()->count() > 0) {
        return;
    }
    // Deleted the last item on the root node, shrink the height of the tree.
    if (root()->is_leaf()) {
        assert(size() == 0);
        delete_leaf_node(root());
        *mutable_root() = nullptr;
    } else {
        node_type* child = root()->child(0);
        if (child->is_leaf()) {
            // The child is a leaf node so simply make it the root node in the tree.
            child->make_root();
            delete_internal_root_node();
            *mutable_root() = child;
        } else {
            // The child is an internal node. We want to keep the existing root node
            // so we move all of the values from the child node into the existing
            // (empty) root node.
            child->swap(get_allocator(), root());
            delete_internal_node(child);
        }
    }
}

template <typename P>
template <typename IterType>
inline IterType btree<P>::internal_last(IterType iter)
{
    while (iter.node && iter.position == iter.node->count()) {
        iter.position = iter.node->position();
        iter.node = iter.node->parent();
        if (iter.node->is_leaf()) {
            iter.node = nullptr;
        }
    }
    return iter;
}

template <typename P>
template <typename... Args>
inline typename btree<P>::iterator btree<P>::internal_emplace(iterator iter, Args&&... args)
{
    if (!iter.node->is_leaf()) {
        // We can't insert on an internal node. Instead, we'll insert after the
        // previous value which is guaranteed to be on a leaf node.
        --iter;
        ++iter.position;
    }
    if (iter.node->count() == iter.node->max_count()) {
        // Make room in the leaf for the new item.
        if (iter.node->max_count() < kNodeValues) {
            // Insertion into the root where the root is smaller that the full node
            // size. Simply grow the size of the root node.
            assert(iter.node == root());
            iter.node = new_leaf_root_node(std::min<int>(kNodeValues, 2 * iter.node->max_count()));
            iter.node->swap(get_allocator(), root());
            delete_leaf_node(root());
            *mutable_root() = iter.node;
        } else {
            rebalance_or_split(&iter);
            ++mutable_size();
        }
    } else if (!root()->is_leaf()) {
        ++mutable_size();
    }
    iter.node->emplace_value(get_allocator(), iter.position, std::forward<Args>(args)...);
    return iter;
}

template <typename P>
template <typename IterType>
inline std::pair<IterType, match_kind> btree<P>::internal_locate(fast_key_type key,
                                                                 IterType iter) const
{
    using search_type = decltype(iter.node->lower_bound(key, key_comp()));
    if (iter.node) {
        for (;;) {
            const search_type res = iter.node->lower_bound(key, key_comp());
            iter.position = res.value;
            if (res.is_exact_match()) {
                return std::make_pair(iter, match_kind::exact);
            }
            if (iter.node->is_leaf()) {
                break;
            }
            iter.node = iter.node->child(iter.position);
        }
        // We were using a strict-weak ordered function, so we don't know
        // whether we found a real thing, or not. We have to verify
        if (!search_type::has_exact_match()) {
            auto last = internal_last(iter);
            if (last.node && !compare_keys(key, last.key())) {
                return std::make_pair(last, match_kind::exact);
            }
        }
    }
    return std::make_pair(iter, match_kind::not_exact);
}

template <typename P>
template <typename IterType>
inline IterType btree<P>::internal_lower_bound(fast_key_type key, IterType iter) const
{
    if (iter.node) {
        for (;;) {
            iter.position = iter.node->lower_bound(key, key_comp()).value;
            if (iter.node->is_leaf()) {
                break;
            }
            iter.node = iter.node->child(iter.position);
        }
        iter = internal_last(iter);
    }
    return iter;
}

template <typename P>
template <typename IterType>
inline IterType btree<P>::internal_upper_bound(fast_key_type key, IterType iter) const
{
    if (iter.node) {
        for (;;) {
            iter.position = iter.node->upper_bound(key, key_comp()).value;
            if (iter.node->is_leaf()) {
                break;
            }
            iter.node = iter.node->child(iter.position);
        }
        iter = internal_last(iter);
    }
    return iter;
}

template <typename P>
template <typename IterType>
inline IterType btree<P>::internal_find_unique(fast_key_type key, IterType iter) const
{
    auto res = internal_locate(key, iter);
    return res.second == match_kind::exact ? res.first : IterType(nullptr, 0);
}

template <typename P>
template <typename IterType>
inline IterType btree<P>::internal_find_multi(fast_key_type key, IterType iter) const
{
    if (iter.node) {
        iter = internal_lower_bound(key, iter);
        if (iter.node) {
            iter = internal_last(iter);
            if (iter.node && !compare_keys(key, iter.key())) {
                return iter;
            }
        }
    }
    return IterType(nullptr, 0);
}

template <typename P> inline void btree<P>::internal_clear(node_type* node)
{
    if (!node->is_leaf()) {
        for (int i = 0; i <= node->count(); ++i) {
            internal_clear(node->child(i));
        }
        if (node == root()) {
            delete_internal_root_node();
        } else {
            delete_internal_node(node);
        }
    } else {
        delete_leaf_node(node);
    }
}

template <typename P>
inline void btree<P>::internal_dump(std::ostream& os, const node_type* node, int level) const
{
    for (int i = 0; i < node->count(); ++i) {
        if (!node->is_leaf()) {
            internal_dump(os, node->child(i), level + 1);
        }
        for (int j = 0; j < level; ++j) {
            os << "  ";
        }
        os << node->key(i) << " [" << level << "]\n";
    }
    if (!node->is_leaf()) {
        internal_dump(os, node->child(node->count()), level + 1);
    }
}

template <typename P>
inline int btree<P>::internal_verify(const node_type* node, optional_key lo, optional_key hi) const
{
    assert(node->count() > 0);
    assert(node->count() <= node->max_count());
    if (lo.second) {
        assert(!compare_keys(node->key(0), lo.first));
    }
    if (hi.second) {
        assert(!compare_keys(hi.first, node->key(node->count() - 1)));
    }
    for (int i = 1; i < node->count(); ++i) {
        assert(!compare_keys(node->key(i), node->key(i - 1)));
    }
    int count = node->count();
    if (!node->is_leaf()) {
        for (int i = 0; i <= node->count(); ++i) {
            assert(node->child(i) != nullptr);
            assert(node->child(i)->parent() == node);
            assert(node->child(i)->position() == i);
            count += internal_verify(node->child(i),
                                     (i == 0) ? lo : optional_key(node->key(i - 1), true),
                                     (i == node->count()) ? hi : optional_key(node->key(i), true));
        }
    }
    return count;
}

} // namespace btree

#endif // BTREE_MAP_DETAIL_BTREE_H
