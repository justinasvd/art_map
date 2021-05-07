#ifndef ART_DETAIL_ART_NODES_HEADER_INCLUDED
#define ART_DETAIL_ART_NODES_HEADER_INCLUDED

#include "art_node_base.h"
#include "dump_byte.h"
#include "ffs_nonzero.h"
#include "tree_depth.h"

#if !defined(NDEBUG)
#include <iostream>
#endif

#if defined(__SSE2__)
#include <emmintrin.h>
#include <smmintrin.h>
#endif

#include <algorithm>

namespace art
{
namespace detail
{

// LCOV_EXCL_START
[[noreturn]] inline void cannot_happen(const char* file, int line, const char* func)
{
#ifndef NDEBUG
    std::cerr << "Execution reached an unreachable point at " << file << ':' << line << ": " << func
              << '\n';
    std::abort();
#else
    __builtin_unreachable();
#endif
}
// LCOV_EXCL_STOP

#define CANNOT_HAPPEN() cannot_happen(__FILE__, __LINE__, __func__)

#if defined(__SSE2__)
// Idea from https://stackoverflow.com/a/32945715/80458
inline auto _mm_cmple_epu8(__m128i x, __m128i y) noexcept
{
    return _mm_cmpeq_epi8(_mm_max_epu8(y, x), y);
}
#endif

template <typename Db> class basic_inode_impl : public art_node_base<typename Db::header_type>
{
    using base_t = art_node_base<typename Db::header_type>;

public:
    using inode_type = basic_inode_impl<Db>;
    using inode4_type = basic_inode_4<Db>;
    using inode16_type = basic_inode_16<Db>;
    using inode48_type = basic_inode_48<Db>;
    using inode256_type = basic_inode_256<Db>;

    using header_type = typename Db::header_type;
    using leaf_type = typename Db::leaf_type;
    using bitwise_key = typename Db::bitwise_key;
    using key_size_type = typename bitwise_key::size_type;
    using node_ptr = base_t*;

    using leaf_unique_ptr = unique_node_ptr<leaf_type, Db>;
    using node_unique_ptr = unique_node_ptr<base_t, Db>;

public:
    constexpr void prepend_key_prefix(const basic_inode_impl& prefix1,
                                      std::uint8_t prefix2) noexcept
    {
        /*assert(key_prefix_length() + prefix1.key_prefix_length() < key_prefix_capacity);

        const auto type = static_cast<std::uint8_t>(this->type());
        const auto prefix_u64 = header_as_uint64() & key_bytes_mask;
        const auto trailing_prefix_shift = (prefix1.key_prefix_length() + 1U) * 8U;
        const auto shifted_prefix_u64 = prefix_u64 << trailing_prefix_shift;
        const auto shifted_prefix2 = static_cast<std::uint64_t>(prefix2) << trailing_prefix_shift;
        const auto prefix1_mask = ((1ULL << trailing_prefix_shift) - 1) ^ 0xFFU;
        const auto masked_prefix1 = prefix1.header_as_uint64() & prefix1_mask;
        const auto prefix_result = shifted_prefix_u64 | shifted_prefix2 | masked_prefix1 | type;
        set_header(prefix_result);

        f.f.key_prefix_length =
            static_cast<key_prefix_size>(key_prefix_length() + prefix1.key_prefix_length() + 1);*/
    }

    [[gnu::cold, gnu::noinline]] void dump(std::ostream& os) const
    {
        os << " parent = " << parent() << " #children = "
           << (children_count == 0 ? 256 : static_cast<unsigned>(children_count));
        switch (this->type()) {
        case node_type::I4:
            static_cast<const inode4_type*>(this)->dump(os);
            break;
        case node_type::I16:
            static_cast<const inode16_type*>(this)->dump(os);
            break;
        case node_type::I48:
            static_cast<const inode48_type*>(this)->dump(os);
            break;
        case node_type::I256:
            static_cast<const inode256_type*>(this)->dump(os);
            break;
            // LCOV_EXCL_START
        case node_type::LEAF:
            CANNOT_HAPPEN();
            // LCOV_EXCL_STOP
        }
    }

    // Don't move the unique leaf pointer by value, only by reference. Otherwise,
    // on failure to insert you won't have a leaf pointer anymore
    [[nodiscard]] constexpr bool add(leaf_unique_ptr& child) noexcept
    {
        assert(child != nullptr);
        assert(child->prefix_length() != 0);

        switch (this->type()) {
        case node_type::I4:
            return add_if_not_full<inode4_type>(child);
        case node_type::I16:
            return add_if_not_full<inode16_type>(child);
        case node_type::I48:
            return add_if_not_full<inode48_type>(child);
        case node_type::I256:
            return add_if_not_full<inode256_type>(child);
        default:
            CANNOT_HAPPEN();
        }
    }

    [[nodiscard]] constexpr bool remove(std::uint8_t child_index, Db& db_instance) noexcept
    {
        switch (this->type()) {
        case node_type::I4:
            return remove_if_not_minsize<inode4_type>(child_index, db_instance);
        case node_type::I16:
            return remove_if_not_minsize<inode16_type>(child_index, db_instance);
        case node_type::I48:
            return remove_if_not_minsize<inode48_type>(child_index, db_instance);
        case node_type::I256:
            return remove_if_not_minsize<inode256_type>(child_index, db_instance);
        default:
            CANNOT_HAPPEN();
        }
    }

    constexpr void replace(std::uint8_t child_index, node_unique_ptr child) noexcept
    {
        switch (this->type()) {
        case node_type::I4:
            static_cast<inode4_type*>(this)->replace(child_index, std::move(child));
            break;
        case node_type::I16:
            static_cast<inode16_type*>(this)->replace(child_index, std::move(child));
            break;
        case node_type::I48:
            static_cast<inode48_type*>(this)->replace(child_index, std::move(child));
            break;
        case node_type::I256:
            static_cast<inode256_type*>(this)->replace(child_index, std::move(child));
            break;
            // LCOV_EXCL_START
        case node_type::LEAF:
            CANNOT_HAPPEN();
            // LCOV_EXCL_STOP
        }
    }

    constexpr void delete_subtree(Db& db_instance) noexcept
    {
        switch (this->type()) {
        case node_type::I4:
            return static_cast<inode4_type*>(this)->delete_subtree(db_instance);
        case node_type::I16:
            return static_cast<inode16_type*>(this)->delete_subtree(db_instance);
        case node_type::I48:
            return static_cast<inode48_type*>(this)->delete_subtree(db_instance);
        case node_type::I256:
            return static_cast<inode256_type*>(this)->delete_subtree(db_instance);
            // LCOV_EXCL_START
        case node_type::LEAF:
            CANNOT_HAPPEN();
            // LCOV_EXCL_STOP
        }
    }

    [[nodiscard]] constexpr typename Db::const_iterator find_child(std::uint8_t key_byte) noexcept
    {
        switch (this->type()) {
        case node_type::I4:
            return static_cast<inode4_type*>(this)->find_child(key_byte);
        case node_type::I16:
            return static_cast<inode16_type*>(this)->find_child(key_byte);
        case node_type::I48:
            return static_cast<inode48_type*>(this)->find_child(key_byte);
        case node_type::I256:
            return static_cast<inode256_type*>(this)->find_child(key_byte);
            // LCOV_EXCL_START
        case node_type::LEAF:
            CANNOT_HAPPEN();
        }
        CANNOT_HAPPEN();
        // LCOV_EXCL_STOP
    }

    static void dump(std::ostream& os, const node_ptr node)
    {
        os << "node at: " << node;
        if (node == nullptr) {
            os << '\n';
            return;
        }

        os << ", type = ";
        node->dump(os);
        if (node->type() == node_type::LEAF) {
            os << ", value = " << static_cast<const leaf_type*>(node)->value() << '\n';
        } else {
            // Print inode base info
            static_cast<inode_type*>(node)->dump(os);
        }
    }

protected:
    constexpr basic_inode_impl(node_type type, unsigned min_size, bitwise_key key,
                               key_size_type key_size, inode_type* parent = nullptr) noexcept
        : base_t(assert_non_leaf(type), key, key_size)
        , parent_(parent)
        , children_count(min_size)
    {
    }

    template <typename SourceNode>
    constexpr basic_inode_impl(node_type type, unsigned min_size, const SourceNode& node) noexcept
        : base_t(assert_non_leaf(type), node.prefix())
        , parent_(node.parent())
        , children_count(min_size)
    {
    }

    inode_type* parent() const noexcept { return parent_; }

    void reparent(node_ptr node) noexcept
    {
        if (node->type() != node_type::LEAF) {
            static_cast<inode_type*>(node)->parent_ = this;
        }
    }

private:
    [[nodiscard]] static constexpr node_type assert_non_leaf(node_type type) noexcept
    {
        assert(type != node_type::LEAF);
        return type;
    }

    template <typename Node>
    [[nodiscard]] constexpr bool add_if_not_full(leaf_unique_ptr& child) noexcept
    {
        Node* const dst = static_cast<Node*>(this);
        return !dst->is_full() ? (dst->add(std::move(child)), true) : false;
    }

    template <typename Node>
    [[nodiscard]] constexpr bool remove_if_not_minsize(std::uint8_t child_index,
                                                       Db& db_instance) noexcept
    {
        Node* const dst = static_cast<Node*>(this);
        return !dst->is_min_size() ? (dst->remove(child_index, db_instance), true) : false;
    }

protected:
    inode_type* parent_;
    std::uint8_t children_count;
};

template <typename Db, unsigned MinSize, unsigned Capacity, node_type NodeType,
          class SmallerDerived, class LargerDerived, class Derived>
class basic_inode : public basic_inode_impl<Db>
{
    static_assert(NodeType != node_type::LEAF);
    static_assert(!std::is_same<Derived, LargerDerived>::value);
    static_assert(!std::is_same<SmallerDerived, Derived>::value);
    static_assert(!std::is_same<SmallerDerived, LargerDerived>::value);
    static_assert(MinSize < Capacity);

    using parent_type = basic_inode_impl<Db>;

public:
    using header_type = typename parent_type::header_type;
    using bitwise_key = typename parent_type::bitwise_key;
    using key_size_type = typename bitwise_key::size_type;
    using leaf_unique_ptr = typename parent_type::leaf_unique_ptr;
    using node_unique_ptr = typename parent_type::node_unique_ptr;

    constexpr basic_inode(bitwise_key key, key_size_type key_size,
                          parent_type* parent = nullptr) noexcept
        : parent_type(NodeType, MinSize, key, key_size, parent)
    {
    }

    explicit constexpr basic_inode(const std::pair<bitwise_key, key_size_type>& prefix,
                                   parent_type* parent = nullptr) noexcept
        : parent_type(NodeType, MinSize, prefix.first, prefix.second, parent)
    {
    }

    [[nodiscard]] static constexpr auto create(std::unique_ptr<LargerDerived>&& source_node,
                                               std::uint8_t child_to_delete, Db& db_instance)
    {
        return std::make_unique<Derived>(std::move(source_node), child_to_delete, db_instance);
    }

    [[nodiscard]] static constexpr auto create(std::unique_ptr<SmallerDerived>&& source_node,
                                               leaf_unique_ptr&& child, tree_depth depth)
    {
        return std::make_unique<Derived>(std::move(source_node), std::move(child), depth);
    }

    [[nodiscard]] constexpr bool is_full() const noexcept
    {
        assert(this->type() == NodeType);
        return this->children_count == capacity;
    }

    [[nodiscard]] constexpr bool is_min_size() const noexcept
    {
        assert(this->type() == NodeType);
        return this->children_count == min_size;
    }

    static constexpr auto min_size = MinSize;
    static constexpr auto capacity = Capacity;
    static constexpr auto static_node_type = NodeType;

    using smaller_derived_type = SmallerDerived;
    using inode_type = typename basic_inode_impl<Db>::inode_type;

protected:
    constexpr basic_inode(bitwise_key k1, bitwise_key k2, tree_depth depth) noexcept
        : basic_inode_impl<Db>{NodeType, MinSize, k1, k2, depth}
    {
        assert(is_min_size());
    }

    constexpr basic_inode(unsigned key_prefix_len,
                          const inode_type& key_prefix_source_node) noexcept
        : basic_inode_impl<Db>{NodeType, MinSize, key_prefix_len, key_prefix_source_node}
    {
        assert(is_min_size());
    }

    explicit constexpr basic_inode(const SmallerDerived& source_node) noexcept
        : basic_inode_impl<Db>(NodeType, MinSize, source_node)
    {
        assert(source_node.is_full());
        assert(is_min_size());
    }

    explicit constexpr basic_inode(const LargerDerived& source_node) noexcept
        : basic_inode_impl<Db>(NodeType, Capacity, source_node)
    {
        assert(source_node.is_min_size());
        assert(is_full());
    }
};

// A class used as a sentinel for basic_inode template args: the
// larger node type for the largest node type and the smaller node type for
// the smallest node type.
class fake_inode final
{
public:
    fake_inode() = delete;
};

template <typename Db>
using basic_inode_4_parent =
    basic_inode<Db, 2, 4, node_type::I4, fake_inode, basic_inode_16<Db>, basic_inode_4<Db>>;

template <typename Db> class basic_inode_4 : public basic_inode_4_parent<Db>
{
    using parent_type = basic_inode_4_parent<Db>;
    using bitwise_key = typename parent_type::bitwise_key;
    using key_size_type = typename bitwise_key::size_type;
    using inode4_type = typename parent_type::inode4_type;
    using inode16_type = typename parent_type::inode16_type;

    using node_ptr = typename parent_type::node_ptr;
    using leaf_type = typename parent_type::leaf_type;
    using leaf_unique_ptr = typename parent_type::leaf_unique_ptr;
    using node_unique_ptr = typename parent_type::node_unique_ptr;

public:
    using parent_type::basic_inode;

    constexpr basic_inode_4(std::unique_ptr<inode16_type> source_node, std::uint8_t child_to_delete,
                            // cppcheck-suppress constParameter
                            Db& db_instance)
        : parent_type{*source_node}
    {
        const auto* source_keys_itr = source_node->keys.byte_array.cbegin();
        auto* keys_itr = keys.byte_array.begin();
        const auto* source_children_itr = source_node->children.cbegin();
        auto* children_itr = children.begin();

        while (source_keys_itr != source_node->keys.byte_array.cbegin() + child_to_delete) {
            *keys_itr++ = *source_keys_itr++;
            *children_itr++ = *source_children_itr++;
        }

        auto reclaim_on_scope_exit = db_instance.make_unique_node_ptr(*source_children_itr);

        ++source_keys_itr;
        ++source_children_itr;

        while (source_keys_itr != source_node->keys.byte_array.cbegin() + inode16_type::min_size) {
            *keys_itr++ = *source_keys_itr++;
            *children_itr++ = *source_children_itr++;
        }

        assert(std::is_sorted(keys.byte_array.cbegin(),
                              keys.byte_array.cbegin() + this->children_count));
    }

    void add_two_to_empty(node_ptr child1, leaf_unique_ptr&& child2) noexcept
    {
        const key_size_type trim = this->prefix_length();
        child1->shift_right(trim);
        child2->shift_right(trim);
        add_two_to_empty(child1->pop_front(), child1, child2->pop_front(), std::move(child2));
    }

    constexpr void add(leaf_unique_ptr child) noexcept
    {
        assert(this->type() == basic_inode_4::static_node_type);
        assert(child->prefix_length() != 0);

        auto children_count = this->children_count;

        assert(std::is_sorted(keys.byte_array.cbegin(), keys.byte_array.cbegin() + children_count));

        const auto key_byte = child->pop_front();

        const auto first_lt = ((keys.integer & 0xFFU) < key_byte) ? 1 : 0;
        const auto second_lt = (((keys.integer >> 8U) & 0xFFU) < key_byte) ? 1 : 0;
        const auto third_lt =
            ((children_count == 3) && ((keys.integer >> 16U) & 0xFFU) < key_byte) ? 1 : 0;
        const auto insert_pos_index = static_cast<unsigned>(first_lt + second_lt + third_lt);

        for (typename decltype(keys.byte_array)::size_type i = children_count; i > insert_pos_index;
             --i) {
            keys.byte_array[i] = keys.byte_array[i - 1];
            // TODO(laurynas): Node4 children fit into a single YMM register on AVX
            // onwards, see if it is possible to do shift/insert with it. Checked
            // plain AVX, it seems that at least AVX2 is required.
            children[i] = children[i - 1];
        }
        keys.byte_array[insert_pos_index] = static_cast<std::uint8_t>(key_byte);
        children[insert_pos_index] = child.release();

        ++children_count;
        this->children_count = children_count;

        assert(std::is_sorted(keys.byte_array.cbegin(), keys.byte_array.cbegin() + children_count));
    }

    constexpr void remove(std::uint8_t child_index, Db& db_instance) noexcept
    {
        assert(this->type() == basic_inode_4::static_node_type);

        auto children_count = this->children_count;

        assert(child_index < children_count);
        assert(std::is_sorted(keys.byte_array.cbegin(), keys.byte_array.cbegin() + children_count));

        auto reclaim_on_scope_exit = db_instance.make_unique_node_ptr(children[child_index]);

        for (typename decltype(keys.byte_array)::size_type i = child_index;
             i < static_cast<unsigned>(this->children_count - 1); ++i) {
            // TODO(laurynas): see the AVX2 TODO at add method
            keys.byte_array[i] = keys.byte_array[i + 1];
            children[i] = children[i + 1];
        }

        --children_count;
        this->children_count = children_count;

        assert(std::is_sorted(keys.byte_array.cbegin(), keys.byte_array.cbegin() + children_count));
    }

    constexpr auto leave_last_child(std::uint8_t child_to_delete, Db& db_instance) noexcept
    {
        /*assert(this->is_min_size());
        assert(child_to_delete == 0 || child_to_delete == 1);
        assert(this->type() == basic_inode_4::static_node_type);

        const auto child_to_delete_ptr = children[child_to_delete];
        const std::uint8_t child_to_leave = (child_to_delete == 0) ? 1 : 0;
        const auto child_to_leave_ptr = children[child_to_leave];
        node_unique_ptr reclaim_on_scope_exit{child_to_delete_ptr, db_instance};
        if (child_to_leave_ptr.type() != node_type::LEAF) {
            child_to_leave_ptr.internal->prepend_key_prefix(*this, keys.byte_array[child_to_leave]);
        }
        return child_to_leave_ptr;*/
    }

    [[nodiscard, gnu::pure]] typename Db::const_iterator find_child(std::uint8_t key_byte) noexcept
    {
        using iterator = typename Db::const_iterator;

#if defined(__SSE2__)
        const auto replicated_search_key = _mm_set1_epi8(static_cast<char>(key_byte));
        const auto keys_in_sse_reg = _mm_cvtsi32_si128(static_cast<std::int32_t>(keys.integer));
        const auto matching_key_positions = _mm_cmpeq_epi8(replicated_search_key, keys_in_sse_reg);
        const auto mask = (1U << this->children_count) - 1;
        const auto bit_field =
            static_cast<unsigned>(_mm_movemask_epi8(matching_key_positions)) & mask;
        if (bit_field != 0) {
            const auto i = static_cast<unsigned>(__builtin_ctz(bit_field));
            return iterator(children[i], i, this);
        }
#else  // No SSE
       // Bit twiddling:
       // contains_byte:     __builtin_ffs:   for key index:
       //    0x80000000               0x20                3
       //      0x800000               0x18                2
       //      0x808000               0x10                1
       //          0x80                0x8                0
       //           0x0                0x0        not found
        const auto result = static_cast<decltype(keys.byte_array)::size_type>(
            // __builtin_ffs takes signed argument:
            // NOLINTNEXTLINE(hicpp-signed-bitwise)
            __builtin_ffs(static_cast<std::int32_t>(contains_byte(keys.integer, key_byte))) >> 3);

        if ((result == 0) || (result > this->children_count))
            return std::make_pair(0xFF, nullptr);

        return iterator(children[result - 1], result - 1, this);
#endif // __SSE__

        return iterator();
    }

    constexpr void replace(std::uint8_t child_index, node_unique_ptr child) noexcept
    {
        children[child_index] = child.release();
    }

    constexpr void delete_subtree(Db& db_instance) noexcept
    {
        const auto children_count_copy = this->children_count;
        for (std::uint8_t i = 0; i < children_count_copy; ++i) {
            db_instance.delete_subtree(children[i]);
        }
    }

    [[gnu::cold, gnu::noinline]] void dump(std::ostream& os) const
    {
        const auto children_count_copy = this->children_count;
        os << ", key bytes =";
        for (std::uint8_t i = 0; i < children_count_copy; i++)
            dump_byte(os, this->keys.byte_array[i]);
        os << ", children:\n";
        for (std::uint8_t i = 0; i < children_count_copy; i++)
            parent_type::dump(os, children[i]);
    }

protected:
    constexpr void add_two_to_empty(std::uint8_t key1, node_ptr child1, std::uint8_t key2,
                                    leaf_unique_ptr&& child2) noexcept
    {
        assert(key1 != key2);
        assert(this->children_count == 2);

        this->reparent(child1);

        const std::uint8_t key1_i = key1 < key2 ? 0 : 1;
        const std::uint8_t key2_i = key1_i == 0 ? 1 : 0;
        this->keys.byte_array[key1_i] = key1;
        this->children[key1_i] = child1;
        this->keys.byte_array[key2_i] = key2;
        this->children[key2_i] = child2.release();
        keys.byte_array[2] = std::uint8_t{0};
        keys.byte_array[3] = std::uint8_t{0};

        assert(std::is_sorted(this->keys.byte_array.cbegin(),
                              this->keys.byte_array.cbegin() + this->children_count));
    }

    union {
        std::array<std::uint8_t, basic_inode_4::capacity> byte_array;
        std::uint32_t integer;
    } keys;

    std::array<node_ptr, basic_inode_4::capacity> children;

private:
    friend basic_inode_16<Db>;
};

static constexpr std::uint8_t empty_child = 0xFF;

template <typename Db>
using basic_inode_16_parent = basic_inode<Db, 5, 16, node_type::I16, basic_inode_4<Db>,
                                          basic_inode_48<Db>, basic_inode_16<Db>>;

template <typename Db> class basic_inode_16 : public basic_inode_16_parent<Db>
{
private:
    using parent_type = basic_inode_16_parent<Db>;
    using inode4_type = typename parent_type::inode4_type;
    using inode16_type = typename parent_type::inode16_type;
    using inode48_type = typename parent_type::inode48_type;
    using leaf_type = typename parent_type::leaf_type;
    using node_ptr = typename parent_type::node_ptr;
    using node_unique_ptr = typename parent_type::node_unique_ptr;

public:
    using leaf_unique_ptr = typename parent_type::leaf_unique_ptr;

    constexpr basic_inode_16(unique_node_ptr<inode4_type, Db> source_node,
                             leaf_unique_ptr child) noexcept
        : parent_type(*source_node)
    {
        assert(child->prefix_length() != 0);
        const auto key_byte = child->pop_front();

        const auto keys_integer = source_node->keys.integer;
        const auto first_lt = ((keys_integer & 0xFFU) < key_byte) ? 1 : 0;
        const auto second_lt = (((keys_integer >> 8U) & 0xFFU) < key_byte) ? 1 : 0;
        const auto third_lt = (((keys_integer >> 16U) & 0xFFU) < key_byte) ? 1 : 0;
        const auto fourth_lt = (((keys_integer >> 24U) & 0xFFU) < key_byte) ? 1 : 0;
        const auto insert_pos_index =
            static_cast<unsigned>(first_lt + second_lt + third_lt + fourth_lt);

        unsigned i = 0;
        for (; i < insert_pos_index; ++i) {
            keys.byte_array[i] = source_node->keys.byte_array[i];
            children[i] = source_node->children[i];
            this->reparent(children[i]);
        }

        keys.byte_array[i] = static_cast<std::uint8_t>(key_byte);
        children[i] = child.release();
        ++i;

        for (; i <= inode4_type::capacity; ++i) {
            keys.byte_array[i] = source_node->keys.byte_array[i - 1];
            children[i] = source_node->children[i - 1];
            this->reparent(children[i]);
        }
    }

    constexpr basic_inode_16(std::unique_ptr<inode48_type> source_node,
                             std::uint8_t child_to_delete, Db& db_instance) noexcept
        : parent_type{*source_node}
    {
        source_node->remove_child_pointer(child_to_delete, db_instance);
        source_node->child_indices[child_to_delete] = empty_child;

        // TODO(laurynas): consider AVX512 gather?
        unsigned next_child = 0;
        unsigned i = 0;
        while (true) {
            const auto source_child_i = source_node->child_indices[i];
            if (source_child_i != empty_child) {
                keys.byte_array[next_child] = static_cast<std::uint8_t>(i);
                const auto source_child_ptr = source_node->children.pointer_array[source_child_i];
                assert(source_child_ptr != nullptr);
                this->children[next_child] = source_child_ptr;
                ++next_child;
                if (next_child == basic_inode_16::capacity)
                    break;
            }
            assert(i < 255);
            ++i;
        }

        assert(basic_inode_16::capacity == this->children_count);
        assert(std::is_sorted(keys.byte_array.cbegin(),
                              keys.byte_array.cbegin() + basic_inode_16::capacity));
    }

    constexpr void add(leaf_unique_ptr child) noexcept
    {
        assert(this->type() == basic_inode_16::static_node_type);
        assert(child->prefix_length() != 0);

        const auto key_byte = child->pop_front();
        auto children_count = this->children_count;

        assert(std::is_sorted(keys.byte_array.cbegin(), keys.byte_array.cbegin() + children_count));

        const auto insert_pos_index = get_sorted_key_array_insert_position(key_byte);
        if (insert_pos_index != children_count) {
            assert(keys.byte_array[insert_pos_index] != key_byte);
            std::copy_backward(keys.byte_array.cbegin() + insert_pos_index,
                               keys.byte_array.cbegin() + children_count,
                               keys.byte_array.begin() + children_count + 1);
            std::copy_backward(children.begin() + insert_pos_index,
                               children.begin() + children_count,
                               children.begin() + children_count + 1);
        }
        keys.byte_array[insert_pos_index] = key_byte;
        children[insert_pos_index] = child.release();
        ++children_count;
        this->children_count = children_count;

        assert(std::is_sorted(keys.byte_array.cbegin(), keys.byte_array.cbegin() + children_count));
    }

    constexpr void remove(std::uint8_t child_index, Db& db_instance) noexcept
    {
        assert(this->type() == basic_inode_16::static_node_type);
        auto children_count = this->children_count;
        assert(child_index < children_count);
        assert(std::is_sorted(keys.byte_array.cbegin(), keys.byte_array.cbegin() + children_count));

        auto reclaim_on_scope_exit = db_instance.make_unique_node_ptr(children[child_index]);

        for (unsigned i = child_index + 1; i < children_count; ++i) {
            keys.byte_array[i - 1] = keys.byte_array[i];
            children[i - 1] = children[i];
        }

        --children_count;
        this->children_count = children_count;

        assert(std::is_sorted(keys.byte_array.cbegin(), keys.byte_array.cbegin() + children_count));
    }

    [[nodiscard, gnu::pure]] constexpr typename Db::const_iterator find_child(
        std::uint8_t key_byte) noexcept
    {
        using iterator = typename Db::const_iterator;

#if defined(__SSE2__)
        const auto replicated_search_key = _mm_set1_epi8(static_cast<char>(key_byte));
        const auto matching_key_positions = _mm_cmpeq_epi8(replicated_search_key, this->keys.sse);
        const auto mask = (1U << this->children_count) - 1;
        const auto bit_field =
            static_cast<unsigned>(_mm_movemask_epi8(matching_key_positions)) & mask;
        if (bit_field != 0) {
            const auto i = static_cast<unsigned>(__builtin_ctz(bit_field));
            return iterator(this->children[i], i, this);
        }
#else
#error Needs porting
#endif
        return iterator();
    }

    constexpr void replace(std::uint8_t child_index, node_unique_ptr child) noexcept
    {
        children[child_index] = child.release();
    }

    constexpr void delete_subtree(Db& db_instance) noexcept
    {
        const auto children_count = this->children_count;
        for (std::uint8_t i = 0; i < children_count; ++i)
            db_instance.delete_subtree(this->children[i]);
    }

    [[gnu::cold, gnu::noinline]] void dump(std::ostream& os) const
    {
        const auto children_count = this->children_count;
        os << ", key bytes =";
        for (std::uint8_t i = 0; i < children_count; ++i)
            dump_byte(os, this->keys.byte_array[i]);
        os << ", children:\n";
        for (std::uint8_t i = 0; i < children_count; ++i)
            parent_type::dump(os, this->children[i]);
    }

private:
    [[nodiscard, gnu::pure]] constexpr auto get_sorted_key_array_insert_position(
        std::uint8_t key_byte) noexcept
    {
        const auto children_count = this->children_count;

        assert(children_count < basic_inode_16::capacity);
        assert(std::is_sorted(keys.byte_array.cbegin(), keys.byte_array.cbegin() + children_count));
        assert(std::adjacent_find(keys.byte_array.cbegin(),
                                  keys.byte_array.cbegin() + children_count) >=
               keys.byte_array.cbegin() + children_count);

#if defined(__SSE2__)
        const auto replicated_insert_key = _mm_set1_epi8(static_cast<char>(key_byte));
        const auto lesser_key_positions = _mm_cmple_epu8(replicated_insert_key, keys.sse);
        const auto mask = (1U << children_count) - 1;
        const auto bit_field =
            static_cast<unsigned>(_mm_movemask_epi8(lesser_key_positions)) & mask;
        const auto result =
            static_cast<std::uint8_t>((bit_field != 0) ? __builtin_ctz(bit_field) : children_count);
#else
        const auto result = static_cast<std::uint8_t>(
            std::lower_bound(keys.byte_array.cbegin(),
                             keys.byte_array.cbegin() + children_count_ccopy, key_byte) -
            keys.byte_array.cbegin());
#endif

        assert(result == children_count || keys.byte_array[result] != key_byte);
        return result;
    }

protected:
    union {
        std::array<std::uint8_t, basic_inode_16::capacity> byte_array;
        __m128i sse;
    } keys;
    std::array<node_ptr, basic_inode_16::capacity> children;

private:
    friend basic_inode_4<Db>;
    friend basic_inode_48<Db>;
};

template <typename Db>
using basic_inode_48_parent = basic_inode<Db, 17, 48, node_type::I48, basic_inode_16<Db>,
                                          basic_inode_256<Db>, basic_inode_48<Db>>;

template <typename Db> class basic_inode_48 : public basic_inode_48_parent<Db>
{
    using parent_type = basic_inode_48_parent<Db>;
    using inode16_type = typename parent_type::inode16_type;
    using inode48_type = typename parent_type::inode48_type;
    using inode256_type = typename parent_type::inode256_type;
    using node_ptr = typename parent_type::node_ptr;
    using node_unique_ptr = typename parent_type::node_unique_ptr;

public:
    using leaf_unique_ptr = typename parent_type::leaf_unique_ptr;

    constexpr basic_inode_48(unique_node_ptr<inode16_type, Db> source_node,
                             leaf_unique_ptr child) noexcept
        : parent_type(*source_node)
    {
        auto* const __restrict__ source_node_ptr = source_node.get();
        auto* const __restrict__ child_ptr = child.release();

        // TODO(laurynas): initialize at declaration
        // Cannot use memset without C++20 atomic_ref, but even then check whether
        // this compiles to memset already
        std::fill(this->child_indices.begin(), this->child_indices.end(), empty_child);

        // TODO(laurynas): consider AVX512 scatter?
        std::uint8_t i = 0;
        for (; i < inode16_type::capacity; ++i) {
            const auto existing_key_byte = source_node_ptr->keys.byte_array[i];
            this->child_indices[static_cast<std::uint8_t>(existing_key_byte)] = i;
        }
        for (i = 0; i < inode16_type::capacity; ++i) {
            this->children.pointer_array[i] = source_node_ptr->children[i];
            this->reparent(this->children.pointer_array[i]);
        }

        const auto key_byte = child_ptr->pop_front();
        assert(this->child_indices[key_byte] == empty_child);
        this->child_indices[key_byte] = i;
        this->children.pointer_array[i] = child_ptr;
        for (i = this->children_count; i < basic_inode_48::capacity; i++) {
            this->children.pointer_array[i] = nullptr;
        }
    }

    constexpr basic_inode_48(unique_node_ptr<inode256_type, Db> source_node,
                             std::uint8_t child_to_delete,
                             // cppcheck-suppress constParameter
                             Db& db_instance) noexcept
        : parent_type{*source_node}
    {
        auto* const __restrict__ source_node_ptr = source_node.get();
        auto reclaim_on_scope_exit =
            db_instance.make_unique_node_ptr(source_node_ptr->children[child_to_delete]);
        source_node_ptr->children[child_to_delete] = nullptr;

        // std::memset(&child_indices[0], empty_child, 256);

        std::uint8_t next_child = 0;
        for (unsigned child_i = 0; child_i < 256; child_i++) {
            const auto child_ptr = source_node_ptr->children[child_i];
            if (child_ptr == nullptr)
                continue;

            this->child_indices[child_i] = next_child;
            this->children.pointer_array[next_child] = source_node_ptr->children[child_i];
            ++next_child;

            if (next_child == this->children_count)
                break;
        }
    }

    constexpr void add(leaf_unique_ptr child) noexcept
    {
        assert(this->type() == basic_inode_48::static_node_type);
        assert(child->prefix_length() != 0);

        const auto key_byte = child->pop_front();
        assert(child_indices[key_byte] == empty_child);
        unsigned i{0};
#if defined(__SSE4_2__)
        const auto nullptr_vector = _mm_setzero_si128();
        while (true) {
            const auto ptr_vec0 = _mm_load_si128(&children.pointer_vector[i]);
            const auto ptr_vec1 = _mm_load_si128(&children.pointer_vector[i + 1]);
            const auto ptr_vec2 = _mm_load_si128(&children.pointer_vector[i + 2]);
            const auto ptr_vec3 = _mm_load_si128(&children.pointer_vector[i + 3]);
            const auto vec0_cmp = _mm_cmpeq_epi64(ptr_vec0, nullptr_vector);
            const auto vec1_cmp = _mm_cmpeq_epi64(ptr_vec1, nullptr_vector);
            const auto vec2_cmp = _mm_cmpeq_epi64(ptr_vec2, nullptr_vector);
            const auto vec3_cmp = _mm_cmpeq_epi64(ptr_vec3, nullptr_vector);
            // OK to treat 64-bit comparison result as 32-bit vector: we need to find
            // the first 0xFF only.
            const auto vec01_cmp = _mm_packs_epi32(vec0_cmp, vec1_cmp);
            const auto vec23_cmp = _mm_packs_epi32(vec2_cmp, vec3_cmp);
            const auto vec_cmp = _mm_packs_epi32(vec01_cmp, vec23_cmp);
            const auto cmp_mask = static_cast<std::uint64_t>(_mm_movemask_epi8(vec_cmp));
            if (cmp_mask != 0) {
                i = (i << 1U) + (ffs_nonzero(cmp_mask) >> 1U);
                break;
            }
            i += 4;
        }
#else  // No SSE4.2 support
        node_ptr child_ptr;
        while (true) {
            child_ptr = children.pointer_array[i];
            if (child_ptr == nullptr)
                break;
            assert(i < 255);
            ++i;
        }
#endif // #if defined(__SSE4_2__)
        assert(children.pointer_array[i] == nullptr);
        child_indices[key_byte] = static_cast<std::uint8_t>(i);
        children.pointer_array[i] = child.release();
        ++this->children_count;
    }

    constexpr void remove(std::uint8_t child_index, Db& db_instance) noexcept
    {
        assert(this->type() == basic_inode_48::static_node_type);

        remove_child_pointer(child_index, db_instance);
        children.pointer_array[child_indices[child_index]] = nullptr;
        child_indices[child_index] = empty_child;
        --this->children_count;
    }

    [[nodiscard]] constexpr typename Db::const_iterator find_child(std::uint8_t key_byte) noexcept
    {
        using iterator = typename Db::const_iterator;

        if (this->child_indices[static_cast<std::uint8_t>(key_byte)] != empty_child) {
            const auto child_i = this->child_indices[static_cast<std::uint8_t>(key_byte)];
            return iterator(this->children.pointer_array[child_i],
                            static_cast<std::uint8_t>(key_byte), this);
        }
        return iterator();
    }

    constexpr void replace(std::uint8_t key_byte, node_unique_ptr child) noexcept
    {
        const auto child_i = this->child_indices[static_cast<std::uint8_t>(key_byte)];
        children.pointer_array[child_i] = child.release();
    }

    constexpr void delete_subtree(Db& db_instance) noexcept
    {
        const auto children_count = this->children_count;

        unsigned actual_children_count = 0;
        for (unsigned i = 0; i < this->capacity; ++i) {
            const auto child = this->children.pointer_array[i];
            if (child != nullptr) {
                ++actual_children_count;
                db_instance.delete_subtree(child);
                assert(actual_children_count <= children_count);
            }
        }
        assert(actual_children_count == children_count);
    }

    [[gnu::cold, gnu::noinline]] void dump(std::ostream& os) const
    {
        const auto children_count = this->children_count;

        os << ", key bytes & child indices\n";
        unsigned actual_children_count = 0;
        for (unsigned i = 0; i < 256; i++)
            if (this->child_indices[i] != empty_child) {
                ++actual_children_count;
                os << " ";
                dump_byte(os, static_cast<std::uint8_t>(i));
                os << ", child index = " << static_cast<unsigned>(this->child_indices[i]) << ": ";
                assert(this->children.pointer_array[this->child_indices[i]] != nullptr);
                parent_type::dump(os, this->children.pointer_array[this->child_indices[i]]);
                assert(actual_children_count <= children_count);
            }

        assert(actual_children_count == children_count);
    }

private:
    constexpr void remove_child_pointer(std::uint8_t child_index, Db& db_instance) noexcept
    {
        direct_remove_child_pointer(child_indices[child_index], db_instance);
    }

    constexpr void direct_remove_child_pointer(std::uint8_t children_i,
                                               // cppcheck-suppress constParameter
                                               Db& db_instance) noexcept
    {
        const auto child_ptr = children.pointer_array[children_i];

        assert(children_i != empty_child);
        assert(child_ptr != nullptr);

        auto reclaim_on_scope_exit = db_instance.make_unique_node_ptr(child_ptr);
    }

    std::array<std::uint8_t, 256> child_indices;
    union children_union {
        std::array<node_ptr, basic_inode_48::capacity> pointer_array;
#if defined(__SSE2__)
        static_assert(basic_inode_48::capacity % 2 == 0);
        // To support unrolling without remainder
        static_assert((basic_inode_48::capacity / 2) % 4 == 0);
        // No std::array below because it would ignore the alignment attribute
        // NOLINTNEXTLINE(modernize-avoid-c-arrays)
        __m128i pointer_vector[basic_inode_48::capacity / 2]; // NOLINT(runtime/arrays)
#endif
        children_union() {}
    } children;

    friend basic_inode_16<Db>;
    friend basic_inode_256<Db>;
};

template <typename Db>
using basic_inode_256_parent =
    basic_inode<Db, 49, 256, node_type::I256, basic_inode_48<Db>, fake_inode, basic_inode_256<Db>>;

template <typename Db> class basic_inode_256 : public basic_inode_256_parent<Db>
{
private:
    using parent_type = basic_inode_256_parent<Db>;
    using inode48_type = typename parent_type::inode48_type;
    using inode256_type = typename parent_type::inode256_type;
    using node_ptr = typename parent_type::node_ptr;
    using node_unique_ptr = typename parent_type::node_unique_ptr;

public:
    using leaf_unique_ptr = typename parent_type::leaf_unique_ptr;

    constexpr basic_inode_256(unique_node_ptr<inode48_type, Db> source_node,
                              leaf_unique_ptr child) noexcept
        : parent_type(*source_node)
    {
        assert(child->prefix_length() != 0);

        unsigned children_copied = 0;
        unsigned i = 0;
        while (true) {
            const auto children_i = source_node->child_indices[i];
            if (children_i == empty_child) {
                children[i] = nullptr;
            } else {
                children[i] = source_node->children.pointer_array[children_i];
                this->reparent(children[i]);
                ++children_copied;
                if (children_copied == inode48_type::capacity)
                    break;
            }
            ++i;
        }

        ++i;
        for (; i < basic_inode_256::capacity; ++i)
            children[i] = nullptr;

        const auto key_byte = child->pop_front();
        assert(children[key_byte] == nullptr);
        children[key_byte] = child.release();
    }

    constexpr void add(leaf_unique_ptr child) noexcept
    {
        assert(this->type() == basic_inode_256::static_node_type);
        assert(!this->is_full());
        assert(child->prefix_length() != 0);

        const auto key_byte = child->pop_front();
        assert(children[key_byte] == nullptr);
        children[key_byte] = child.release();
        ++this->children_count;
    }

    constexpr void remove(std::uint8_t child_index, Db& db_instance) noexcept
    {
        const auto child_ptr = children[child_index];

        assert(this->type() == basic_inode_256::static_node_type);
        assert(child_ptr != nullptr);

        auto reclaim_on_scope_exit = db_instance.make_unique_node_ptr(child_ptr);

        children[child_index] = nullptr;
        --this->children_count;
    }

    [[nodiscard]] constexpr typename Db::const_iterator find_child(std::uint8_t key_byte) noexcept
    {
        using iterator = typename Db::const_iterator;
        const auto key_int_byte = static_cast<std::uint8_t>(key_byte);
        if (children[key_int_byte] != nullptr)
            return iterator(children[key_int_byte], key_int_byte, this);
        return iterator();
    }

    constexpr void replace(std::uint8_t key_int_byte, node_unique_ptr child) noexcept
    {
        children[key_int_byte] = child.release();
    }

    template <typename Function>
    constexpr void for_each_child(Function func) noexcept(noexcept(func(0, nullptr)))
    {
        const auto children_count = this->children_count;
        std::uint8_t actual_children_count = 0;
        for (unsigned i = 0; i < 256; ++i) {
            const auto child_ptr = children[i];
            if (child_ptr != nullptr) {
                ++actual_children_count;
                func(i, child_ptr);
                assert(actual_children_count <= children_count || children_count == 0);
            }
        }
        assert(actual_children_count == children_count);
    }

    template <typename Function>
    constexpr void for_each_child(Function func) const noexcept(noexcept(func(0, nullptr)))
    {
        const_cast<basic_inode_256*>(this)->for_each_child(func);
    }

    constexpr void delete_subtree(Db& db_instance) noexcept
    {
        for_each_child([&db_instance](unsigned, node_ptr child) noexcept {
            db_instance.delete_subtree(child);
        });
    }

    [[gnu::cold, gnu::noinline]] void dump(std::ostream& os) const
    {
        os << ", key bytes & children:\n";
        for_each_child([&](unsigned i, node_ptr child) noexcept {
            os << ' ';
            dump_byte(os, static_cast<std::uint8_t>(i));
            os << ' ';
            parent_type::dump(os, child);
        });
    }

private:
    std::array<node_ptr, basic_inode_256::capacity> children;

    friend inode48_type;
};

} // namespace detail
} // namespace art

#endif // ART_DETAIL_ART_NODES_HEADER_INCLUDED
