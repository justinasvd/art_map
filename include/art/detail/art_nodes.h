#ifndef ART_DETAIL_ART_NODES_HEADER_INCLUDED
#define ART_DETAIL_ART_NODES_HEADER_INCLUDED

#include "art_node_base.h"
#include "dump_byte.h"
#include "tzcnt.h"

#if !defined(NDEBUG)
#include <iostream>
#endif

#if defined(__SSE2__)
#include <emmintrin.h>
#include <smmintrin.h>
#endif

#include <algorithm>
#include <array>

#include <boost/core/ignore_unused.hpp>

namespace art
{
namespace detail
{

[[noreturn]] inline void cannot_happen(const char* file, int line, const char* func)
{
#ifndef NDEBUG
    std::cerr << "Execution reached an unreachable point at " << file << ':' << line << ": " << func
              << '\n';
    std::abort();
#else
    boost::ignore_unused(file);
    boost::ignore_unused(line);
    boost::ignore_unused(func);
    __builtin_unreachable();
#endif
}

#define ART_DETAIL_CANNOT_HAPPEN() cannot_happen(__FILE__, __LINE__, __func__)

#if defined(__SSE2__)
namespace sse2
{
// Idea from https://stackoverflow.com/a/32945715/80458
inline __m128i less_or_equal_epu8(__m128i x, __m128i y) noexcept
{
    return _mm_cmpeq_epi8(_mm_min_epu8(y, x), y);
}
inline __m128i greater_or_equal_epu8(__m128i x, __m128i y) noexcept
{
    return _mm_cmpeq_epi8(_mm_max_epu8(y, x), y);
}

inline unsigned int clamped_pos(__m128i x, std::uint8_t clamp) noexcept
{
    // This is not as inefficient as it seems. If sufficiently advanced instruction
    // set is used by the compiler, then the result of _mm_movemask_epi8 will be
    // clamped by a single bzhi instruction.
    return static_cast<unsigned int>(_mm_movemask_epi8(x)) & ((1u << clamp) - 1u);
}

inline unsigned int key_equal_mask(__m128i x, std::uint8_t y, std::uint8_t clamp) noexcept
{
    return clamped_pos(_mm_cmpeq_epi8(x, _mm_set1_epi8(y)), clamp);
}
inline unsigned int key_upper_bound(__m128i x, std::uint8_t y, std::uint8_t clamp) noexcept
{
    return _mm_popcnt_u32(clamped_pos(greater_or_equal_epu8(x, _mm_set1_epi8(y)), clamp));
}
inline unsigned int key_lower_bound(__m128i x, std::uint8_t y) noexcept
{
    return tzcnt(_mm_movemask_epi8(less_or_equal_epu8(x, _mm_set1_epi8(y))));
}

} // namespace sse2
#endif // SSE2

template <typename Db> class basic_inode_impl : public art_node_base<typename Db::bitwise_key>
{
    using base_t = art_node_base<typename Db::bitwise_key>;

public:
    using inode_type = basic_inode_impl<Db>;
    using inode4_type = basic_inode_4<Db>;
    using inode16_type = basic_inode_16<Db>;
    using inode48_type = basic_inode_48<Db>;
    using inode256_type = basic_inode_256<Db>;

    using leaf_type = typename Db::leaf_type;
    using bitwise_key = typename Db::bitwise_key;
    using node_ptr = typename Db::node_ptr;

    using leaf_unique_ptr = unique_node_ptr<leaf_type, Db>;
    using iterator = typename Db::iterator;
    using const_iterator = typename Db::const_iterator;

public:
    [[nodiscard]] constexpr unsigned int num_children() const noexcept
    {
        return children_count != 0 ? static_cast<unsigned>(children_count) : 256;
    }

    template <typename Visitor>
    [[nodiscard]] static auto constexpr dispatch_inode(node_ptr node, Visitor vis) noexcept
    {
        assert(node.tag() != node_type::LEAF);
        switch (node.tag()) {
        case node_type::I4:
            return vis(*static_cast<inode4_type*>(node.get()));
        case node_type::I16:
            return vis(*static_cast<inode16_type*>(node.get()));
        case node_type::I48:
            return vis(*static_cast<inode48_type*>(node.get()));
        case node_type::I256:
            return vis(*static_cast<inode256_type*>(node.get()));
        default:
            ART_DETAIL_CANNOT_HAPPEN();
        }
    }

    [[nodiscard]] static constexpr const_iterator find_child(node_ptr node,
                                                             std::uint8_t key_byte) noexcept
    {
        return dispatch_inode(node, [key_byte](auto& inode) { return inode.find_child(key_byte); });
    }

    [[nodiscard]] static constexpr std::pair<const_iterator, std::uint8_t> lower_bound(
        node_ptr node, std::uint8_t key_byte) noexcept
    {
        return dispatch_inode(node,
                              [key_byte](auto& inode) { return inode.lower_bound(key_byte); });
    }

    [[nodiscard]] static constexpr const_iterator leftmost_child(node_ptr node,
                                                                 unsigned int start = 0) noexcept
    {
        return dispatch_inode(node, [start](auto& inode) { return inode.leftmost_child(start); });
    }

    [[nodiscard]] static constexpr const_iterator leftmost_leaf(node_ptr node,
                                                                int start = 0) noexcept
    {
        assert(start >= 0 && start <= 256);
        const_iterator pos(node);
        while (pos.tag() != node_type::LEAF) {
            pos = leftmost_child(pos.node(), start);
            start = 0;
        }
        return pos;
    }

    static void dump(std::ostream& os, const node_ptr node)
    {
        os << "node at: " << node.get();
        if (BOOST_UNLIKELY(node == nullptr)) {
            os << '\n';
            return;
        }

        os << ", type = ";
        switch (node.tag()) {
        case node_type::LEAF:
            os << "LEAF: ";
            static_cast<const leaf_type*>(node.get())->dump(os);
            os << '\n';
            break;
        case node_type::I4:
            os << "I4: ";
            static_cast<const inode4_type*>(node.get())->dump(os);
            break;
        case node_type::I16:
            os << "I16: ";
            static_cast<const inode16_type*>(node.get())->dump(os);
            break;
        case node_type::I48:
            os << "I48: ";
            static_cast<const inode48_type*>(node.get())->dump(os);
            break;
        case node_type::I256:
            os << "I256: ";
            static_cast<const inode256_type*>(node.get())->dump(os);
            break;
        default:
            ART_DETAIL_CANNOT_HAPPEN();
        }
    }

    [[nodiscard]] node_ptr parent() const noexcept { return parent_; }
    [[nodiscard]] std::uint8_t index() const noexcept { return position; }
    [[nodiscard]] std::pair<node_ptr, std::uint8_t> pos_in_parent() const noexcept
    {
        return std::make_pair(parent_, position);
    }

protected:
    constexpr basic_inode_impl(std::uint8_t min_size, bitwise_key key) noexcept
        : base_t(key)
        , parent_{}
        , position{}
        , children_count(min_size)
    {
    }

    static constexpr void index(inode_type& inode, std::uint8_t index) noexcept
    {
        inode.position = index;
    }

    static constexpr void assign_parent(inode_type& inode, node_ptr parent,
                                        std::uint8_t index) noexcept
    {
        inode.parent_ = parent;
        inode.position = index;
    }
    static constexpr void assign_parent(node_ptr inode, node_ptr parent,
                                        std::uint8_t index) noexcept
    {
        assign_parent(*static_cast<inode_type*>(inode.get()), parent, index);
    }

    void dump(std::ostream& os) const
    {
        base_t::dump(os, this->prefix());
        os << ", parent = " << parent_.get() << ", #children = " << num_children();
    }

protected:
    node_ptr parent_;
    std::uint8_t position;       // Position in parent's array
    std::uint8_t children_count; // Number of children in parent's array
};

template <typename Db, unsigned MinSize, unsigned Capacity, node_type NodeType,
          class SmallerDerived, class LargerDerived, class Derived>
class basic_inode : public basic_inode_impl<Db>
{
    static_assert(static_cast<unsigned>(node_type::LEAF) == 0, "Leaf tag must be 0");

    static_assert(NodeType != node_type::LEAF, "An inode cannot have a leaf tag");
    static_assert(!std::is_same<Derived, LargerDerived>::value, "grow(inode) -> inode");
    static_assert(!std::is_same<SmallerDerived, Derived>::value, "shrink(inode) -> inode");
    static_assert(!std::is_same<SmallerDerived, LargerDerived>::value,
                  "shrink(inode) -> grow(inode)");
    static_assert(MinSize < Capacity, "Misconfigured inode capacity: min size >= capacity");

    using parent_type = basic_inode_impl<Db>;

public:
    using bitwise_key = typename parent_type::bitwise_key;
    using node_ptr = typename parent_type::node_ptr;
    using leaf_unique_ptr = typename parent_type::leaf_unique_ptr;

    using smaller_inode_type = SmallerDerived;
    using larger_inode_type = LargerDerived;

    using iterator = typename Db::iterator;

    static constexpr unsigned min_size = MinSize;
    static constexpr unsigned capacity = Capacity;

    explicit constexpr basic_inode(bitwise_key key) noexcept
        : parent_type(MinSize, key)
    {
    }

    [[nodiscard]] constexpr bool is_full() const noexcept
    {
        return this->children_count == capacity;
    }

    [[nodiscard]] constexpr bool is_min_size() const noexcept
    {
        return this->children_count == min_size;
    }

    [[nodiscard]] static constexpr node_type static_type() noexcept { return NodeType; }

    [[nodiscard]] node_ptr tagged_self() noexcept { return node_ptr(this, NodeType); }
    [[nodiscard]] iterator self_iterator() noexcept
    {
        return iterator(tagged_self(), this->position, this->parent_);
    }

protected:
    explicit constexpr basic_inode(const LargerDerived& source_node) noexcept
        : basic_inode_impl<Db>(Capacity, source_node.prefix())
    {
        assert(source_node.is_min_size());
        assert(is_full());
    }

    void reparent(node_ptr node, std::uint8_t index) noexcept
    {
        // This ugly construct here is to make reparenting a branchless operation.
        // The dummy below is a "parent sink" for leaves. All of this is here to avoid
        // writing
        // if (node.tag() != node_type::LEAF) {
        //    parent_type::assign_parent(node, tagged_self(), index);
        // }
        std::aligned_storage_t<sizeof(parent_type), alignof(parent_type)> dev_null;
        parent_type* parent_sink[2] = {reinterpret_cast<parent_type*>(&dev_null),
                                       static_cast<parent_type*>(node.get())};

        parent_type::assign_parent(*parent_sink[node.tag() != node_type::LEAF], tagged_self(),
                                   index);
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

static constexpr std::uint8_t empty_child = 0xFF;

template <std::size_t N>
inline void assign_empty_child(std::array<std::uint8_t, N>& bytes, std::uint8_t pos) noexcept
{
    // GCC 10+ incorrectly reports that we are writing 1 byte into a region of size 0
    // It is unclear how to properly appease the compiler in this scenario, so we simply
    // disable the warning around the "offending" statement
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstringop-overflow"
    bytes[pos] = empty_child;
#pragma GCC diagnostic pop
}

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
    using const_iterator = typename Db::const_iterator;
    using iterator = typename Db::iterator;

private:
    friend Db;

    // Forward parent's c-tors here
    using basic_inode_4_parent<Db>::basic_inode_4_parent;

    [[nodiscard]] constexpr iterator populate(node_ptr child1, leaf_unique_ptr child2,
                                              std::uint8_t key_byte) noexcept
    {
        assert(child1.tag() != node_type::LEAF);
        child1->shift_right(this->prefix_length());
        const std::uint8_t child1_key = child1->front();
        child1->shift_right(1); // Consume front byte
        return add_two_to_empty(child1_key, child1, key_byte, std::move(child2));
    }

    [[nodiscard]] constexpr iterator populate(leaf_type* child1, leaf_unique_ptr child2,
                                              key_size_type offset) noexcept
    {
        const key_size_type trim = offset + this->prefix_length();
        return add_two_to_empty(child1->prefix()[trim], node_ptr(child1, node_type::LEAF),
                                child2->prefix()[trim], std::move(child2));
    }

public:
    constexpr basic_inode_4(const inode16_type& source_node, std::uint8_t child_to_delete)
        : parent_type(source_node)
    {
        const auto* source_keys_itr = source_node.keys.byte_array.cbegin();
        auto* keys_itr = keys.byte_array.begin();
        const auto* source_children_itr = source_node.children.cbegin();
        auto* children_itr = children.begin();

        std::uint8_t index = 0;
        while (source_keys_itr != source_node.keys.byte_array.cbegin() + child_to_delete) {
            *keys_itr++ = *source_keys_itr++;
            *children_itr = *source_children_itr++;
            this->reparent(*children_itr++, index++);
        }

        ++source_keys_itr;
        ++source_children_itr;

        parent_type::index(*this, child_to_delete);

        while (source_keys_itr != source_node.keys.byte_array.cbegin() + inode16_type::min_size) {
            *keys_itr++ = *source_keys_itr++;
            *children_itr = *source_children_itr++;
            this->reparent(*children_itr++, index++);
        }

        assert(std::is_sorted(keys.byte_array.cbegin(),
                              keys.byte_array.cbegin() + this->children_count));
    }

    [[nodiscard]] constexpr iterator add(leaf_unique_ptr child, std::uint8_t key_byte) noexcept
    {
        auto children_count = this->children_count;

        assert(std::is_sorted(keys.byte_array.cbegin(), keys.byte_array.cbegin() + children_count));

        const unsigned insert_pos_index = key_upper_bound(key_byte);

        for (typename decltype(keys.byte_array)::size_type i = children_count; i > insert_pos_index;
             --i) {
            keys.byte_array[i] = keys.byte_array[i - 1];
            // TODO(laurynas): Node4 children fit into a single YMM register on AVX
            // onwards, see if it is possible to do shift/insert with it. Checked
            // plain AVX, it seems that at least AVX2 is required.
            children[i] = children[i - 1];
            this->reparent(children[i - 1], i);
        }
        keys.byte_array[insert_pos_index] = key_byte;
        children[insert_pos_index] = child.release();

        ++children_count;
        this->children_count = children_count;

        assert(std::is_sorted(keys.byte_array.cbegin(), keys.byte_array.cbegin() + children_count));

        return iterator(children[insert_pos_index], insert_pos_index, this->tagged_self());
    }

    constexpr void remove(std::uint8_t child_index) noexcept
    {
        auto children_count = this->children_count;

        assert(child_index < children_count);
        assert(std::is_sorted(keys.byte_array.cbegin(), keys.byte_array.cbegin() + children_count));

        for (typename decltype(keys.byte_array)::size_type i = child_index;
             i < static_cast<unsigned>(this->children_count - 1); ++i) {
            // TODO(laurynas): see the AVX2 TODO at add method
            keys.byte_array[i] = keys.byte_array[i + 1];
            children[i] = children[i + 1];
            this->reparent(children[i + 1], i);
        }

        --children_count;
        assign_empty_child(keys.byte_array, children_count);
        this->children_count = children_count;

        assert(std::is_sorted(keys.byte_array.cbegin(), keys.byte_array.cbegin() + children_count));
    }

    [[nodiscard]] constexpr node_ptr leave_last_child(std::uint8_t child_to_delete) noexcept
    {
        assert(this->is_min_size());
        assert(child_to_delete <= 1);

        const std::uint8_t child_to_leave = 1 - child_to_delete;
        const auto child_to_leave_ptr = children[child_to_leave];

        if (child_to_leave_ptr.tag() != node_type::LEAF) {
            // Now we have to prepend inode_4's prefix to the last remaining node
            child_to_leave_ptr->shift_left(keys.byte_array[child_to_leave]);
            child_to_leave_ptr->shift_left(this->prefix());
            parent_type::assign_parent(child_to_leave_ptr, node_ptr{}, 0);
        }
        return child_to_leave_ptr;
    }

    [[nodiscard, gnu::pure]] const_iterator find_child(std::uint8_t key_byte) noexcept
    {
#if defined(__SSE2__)
        const unsigned int bit_field =
            sse2::key_equal_mask(_mm_cvtsi32_si128(keys.integer), key_byte, this->children_count);
        if (bit_field != 0) {
            const unsigned int i = tzcnt(bit_field);
            return const_iterator(children[i], i, this->tagged_self());
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

        if ((result != 0) && (result <= this->children_count))
            return const_iterator(children[result - 1], result - 1, this->tagged_self());
#endif // __SSE__

        return const_iterator{};
    }

    [[nodiscard]] constexpr const_iterator leftmost_child(unsigned int start) noexcept
    {
        return start < this->children_count
                   ? const_iterator(children[start], start, this->tagged_self())
                   : const_iterator{};
    }

    [[nodiscard]] constexpr std::pair<const_iterator, std::uint8_t> lower_bound(
        std::uint8_t key_byte) noexcept
    {
        const unsigned int insert_pos_index =
            sse2::key_lower_bound(_mm_cvtsi32_si128(keys.integer), key_byte);
        auto pos = leftmost_child(insert_pos_index);
        return std::make_pair(pos, keys.byte_array[pos.index()]);
    }

    constexpr void replace(iterator pos, node_ptr child) noexcept
    {
        const std::uint8_t child_index = pos.index();
        assert(pos.parent() == this->tagged_self() && pos.node() == children[child_index]);
        children[child_index] = child;
        this->reparent(child, child_index);
    }

    constexpr void delete_subtree(Db& db_instance) noexcept
    {
        const auto children_count_copy = this->children_count;
        for (std::uint8_t i = 0; i < children_count_copy; ++i) {
            db_instance.deallocate(children[i]);
        }
    }

    [[gnu::cold, gnu::noinline]] void dump(std::ostream& os) const
    {
        parent_type::dump(os);
        const auto children_count_copy = this->children_count;
        os << ", key bytes =";
        for (std::uint8_t i = 0; i < children_count_copy; i++)
            dump_byte(os, keys.byte_array[i]);
        os << ", children:\n";
        for (std::uint8_t i = 0; i < children_count_copy; i++)
            parent_type::dump(os, children[i]);
    }

private:
    [[nodiscard, gnu::pure]] constexpr unsigned key_upper_bound(
        std::uint8_t key_byte) const noexcept
    {
#if defined(__SSE2__)
        return sse2::key_upper_bound(_mm_cvtsi32_si128(keys.integer), key_byte,
                                     this->children_count);
#else // No SSE2
        const auto first_lt = ((keys.integer & 0xFFU) < key_byte) ? 1 : 0;
        const auto second_lt = (((keys.integer >> 8U) & 0xFFU) < key_byte) ? 1 : 0;
        const auto third_lt = (((keys.integer >> 16U) & 0xFFU) < key_byte) ? 1 : 0;
        const auto fourth_lt = (((keys.integer >> 24U) & 0xFFU) < key_byte) ? 1 : 0;
        return first_lt + second_lt + third_lt + fourth_lt;
#endif
    }

    constexpr iterator add_two_to_empty(std::uint8_t key1, node_ptr child1, std::uint8_t key2,
                                        leaf_unique_ptr&& child2) noexcept
    {
        assert(key1 != key2);
        assert(this->children_count == 2);

        const std::uint8_t key1_i = !(key1 < key2); // if key1 < key2 then 0, otherwise 1
        const std::uint8_t key2_i = 1 - key1_i;
        keys.byte_array[key1_i] = key1;
        children[key1_i] = child1;
        this->reparent(child1, key1_i);

        keys.byte_array[key2_i] = key2;
        children[key2_i] = child2.release();
        keys.byte_array[2] = empty_child;
        keys.byte_array[3] = empty_child;

        assert(std::is_sorted(keys.byte_array.cbegin(),
                              keys.byte_array.cbegin() + this->children_count));

        return iterator(children[key2_i], key2_i, this->tagged_self());
    }

    union {
        std::array<std::uint8_t, basic_inode_4::capacity> byte_array;
        std::uint32_t integer;
    } keys;

    std::array<node_ptr, basic_inode_4::capacity> children;

private:
    friend basic_inode_16<Db>;
};

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
    using node_ptr = typename parent_type::node_ptr;
    using leaf_unique_ptr = typename parent_type::leaf_unique_ptr;
    using const_iterator = typename Db::const_iterator;
    using iterator = typename Db::iterator;

private:
    friend Db;

    using basic_inode_16_parent<Db>::basic_inode_16_parent;

    [[nodiscard]] constexpr iterator populate(unique_node_ptr<inode4_type, Db> source_node,
                                              leaf_unique_ptr child, std::uint8_t key_byte) noexcept
    {
        assert(source_node->is_full());
        assert(this->is_min_size());

        const unsigned insert_pos_index = source_node->key_upper_bound(key_byte);

        unsigned i = 0;
        for (; i < insert_pos_index; ++i) {
            keys.byte_array[i] = source_node->keys.byte_array[i];
            children[i] = source_node->children[i];
            this->reparent(children[i], i);
        }

        keys.byte_array[i] = key_byte;
        children[i] = child.release();
        iterator inserted(children[i], i, this->tagged_self());

        for (++i; i <= inode4_type::capacity; ++i) {
            keys.byte_array[i] = source_node->keys.byte_array[i - 1];
            children[i] = source_node->children[i - 1];
            this->reparent(children[i], i);
        }
        return inserted;
    }

public:
    constexpr basic_inode_16(const inode48_type& source_node, std::uint8_t child_to_delete) noexcept
        : parent_type(source_node)
    {
        source_node.verify_remove_preconditions(child_to_delete);

        // TODO(laurynas): consider AVX512 gather?
        unsigned next_child = 0;
        for (unsigned i = 0; i != 256; ++i) {
            const auto source_child_i = source_node.child_indices[i];
            if (source_child_i != empty_child) {
                if (child_to_delete != i) {
                    keys.byte_array[next_child] = static_cast<std::uint8_t>(i);
                    const auto source_child_ptr = source_node.children[source_child_i];
                    assert(source_child_ptr != nullptr);
                    children[next_child] = source_child_ptr;
                    this->reparent(children[next_child], next_child);
                    ++next_child;
                    if (next_child == basic_inode_16::capacity)
                        break;
                } else {
                    parent_type::index(*this, next_child);
                }
            }
        }

        assert(basic_inode_16::capacity == this->children_count);
        assert(std::is_sorted(keys.byte_array.cbegin(),
                              keys.byte_array.cbegin() + basic_inode_16::capacity));
    }

    [[nodiscard]] constexpr iterator add(leaf_unique_ptr child, std::uint8_t key_byte) noexcept
    {
        auto children_count = this->children_count;

        const auto insert_pos_index = key_upper_bound(key_byte);
        if (insert_pos_index != children_count) {
            assert(keys.byte_array[insert_pos_index] != key_byte);
            std::copy_backward(keys.byte_array.cbegin() + insert_pos_index,
                               keys.byte_array.cbegin() + children_count,
                               keys.byte_array.begin() + children_count + 1);

            for (std::uint8_t i = insert_pos_index; i != children_count; ++i)
                this->reparent(children[i], i + 1);

            std::copy_backward(children.begin() + insert_pos_index,
                               children.begin() + children_count,
                               children.begin() + children_count + 1);
        }
        keys.byte_array[insert_pos_index] = key_byte;
        children[insert_pos_index] = child.release();
        ++children_count;
        this->children_count = children_count;

        assert(std::is_sorted(keys.byte_array.cbegin(), keys.byte_array.cbegin() + children_count));

        return iterator(children[insert_pos_index], insert_pos_index, this->tagged_self());
    }

    constexpr void remove(std::uint8_t child_index) noexcept
    {
        auto children_count = this->children_count;
        assert(child_index < children_count);
        assert(std::is_sorted(keys.byte_array.cbegin(), keys.byte_array.cbegin() + children_count));

        for (unsigned i = child_index + 1; i < children_count; ++i) {
            keys.byte_array[i - 1] = keys.byte_array[i];
            children[i - 1] = children[i];
            this->reparent(children[i], i - 1);
        }

        --children_count;
        this->children_count = children_count;

        assert(std::is_sorted(keys.byte_array.cbegin(), keys.byte_array.cbegin() + children_count));
    }

    [[nodiscard, gnu::pure]] constexpr const_iterator find_child(std::uint8_t key_byte) noexcept
    {
#if defined(__SSE2__)
        const unsigned int bit_field =
            sse2::key_equal_mask(keys.sse, key_byte, this->children_count);
        if (bit_field != 0) {
            const unsigned int i = tzcnt(bit_field);
            return const_iterator(children[i], i, this->tagged_self());
        }
#else
#error Needs porting
#endif
        return const_iterator{};
    }

    [[nodiscard]] constexpr const_iterator leftmost_child(unsigned int start) noexcept
    {
        return start < this->children_count
                   ? const_iterator(children[start], start, this->tagged_self())
                   : const_iterator{};
    }

    [[nodiscard, gnu::pure]] constexpr std::pair<const_iterator, std::uint8_t> lower_bound(
        std::uint8_t key_byte) noexcept
    {
        const unsigned int insert_pos_index = sse2::key_lower_bound(keys.sse, key_byte);
        auto pos = leftmost_child(insert_pos_index);
        return std::make_pair(pos, keys.byte_array[pos.index()]);
    }

    constexpr void replace(iterator pos, node_ptr child) noexcept
    {
        const std::uint8_t child_index = pos.index();
        assert(pos.parent() == this->tagged_self() && pos.node() == children[child_index]);
        children[child_index] = child;
        this->reparent(children[child_index], child_index);
    }

    constexpr void delete_subtree(Db& db_instance) noexcept
    {
        const auto children_count = this->children_count;
        for (std::uint8_t i = 0; i < children_count; ++i)
            db_instance.deallocate(children[i]);
    }

    [[gnu::cold, gnu::noinline]] void dump(std::ostream& os) const
    {
        parent_type::dump(os);
        const auto children_count = this->children_count;
        os << ", key bytes =";
        for (std::uint8_t i = 0; i < children_count; ++i)
            dump_byte(os, keys.byte_array[i]);
        os << ", children:\n";
        for (std::uint8_t i = 0; i < children_count; ++i)
            parent_type::dump(os, children[i]);
    }

private:
    [[nodiscard, gnu::pure]] constexpr std::uint8_t key_upper_bound(
        std::uint8_t key_byte) const noexcept
    {
        const auto children_count = this->children_count;

        assert(children_count < basic_inode_16::capacity);
        assert(std::is_sorted(keys.byte_array.cbegin(), keys.byte_array.cbegin() + children_count));
        assert(std::adjacent_find(keys.byte_array.cbegin(),
                                  keys.byte_array.cbegin() + children_count) >=
               keys.byte_array.cbegin() + children_count);

#if defined(__SSE2__)
        const uint8_t result = sse2::key_upper_bound(keys.sse, key_byte, children_count);
#else
        const std::uint8_t result =
            std::upper_bound(keys.byte_array.cbegin(), keys.byte_array.cbegin() + children_count,
                             key_byte) -
            keys.byte_array.cbegin();
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

template <typename NodePtr, unsigned int Capacity> union children_array {
    std::array<NodePtr, Capacity> pointer_array;

    [[nodiscard]] static constexpr unsigned int size() noexcept { return Capacity; }

    [[nodiscard]] NodePtr& operator[](unsigned int n) noexcept { return pointer_array[n]; }
    [[nodiscard]] const NodePtr operator[](unsigned int n) const noexcept
    {
        return pointer_array[n];
    }
#if defined(__SSE4_2__)
    // To support unrolling without remainder
    static_assert(Capacity % 8 == 0, "children_array capacity must be divisible by 8, otherwise it "
                                     "cannot support unrolling without a remainder");
    // No std::array below because it would ignore the alignment attribute
    // NOLINTNEXTLINE(modernize-avoid-c-arrays)
    static_assert(2 * sizeof(NodePtr) == sizeof(__m128i),
                  "Incompatible architecture, expected 64 bit pointers");
    __m128i pointer_vector[Capacity / 2]; // NOLINT(runtime/arrays)
#endif
};

template <typename NodePtr, unsigned int N, typename Predicate>
[[nodiscard]] inline unsigned int find_first_eq(const children_array<NodePtr, N>& children,
                                                unsigned int start, Predicate pred) noexcept
{
#if defined(__SSE4_2__)
    constexpr unsigned int mod_8 = ~7u;
    const unsigned int bucket_end = std::min((start + 7) & mod_8, N);
    for (; start != bucket_end; ++start) {
        if (pred(children[start]))
            return start;
    }
    constexpr unsigned int max_siz = N / 2;
    const __m128i nullptr_vector = _mm_setzero_si128();
    unsigned int i = bucket_end >> 1;
    for (; i != max_siz; i += 4) {
        const auto vec0_cmp = _mm_cmpeq_epi64(children.pointer_vector[i], nullptr_vector);
        const auto vec1_cmp = _mm_cmpeq_epi64(children.pointer_vector[i + 1], nullptr_vector);
        const auto vec2_cmp = _mm_cmpeq_epi64(children.pointer_vector[i + 2], nullptr_vector);
        const auto vec3_cmp = _mm_cmpeq_epi64(children.pointer_vector[i + 3], nullptr_vector);
        // OK to treat 64-bit comparison result as 32-bit vector: we need to find
        // the first 0xFF only.
        const auto vec01_cmp = _mm_packs_epi32(vec0_cmp, vec1_cmp);
        const auto vec23_cmp = _mm_packs_epi32(vec2_cmp, vec3_cmp);
        const auto vec_cmp = _mm_packs_epi16(vec01_cmp, vec23_cmp);
        const unsigned int cmp_mask = pred(_mm_movemask_epi8(vec_cmp));
        if (cmp_mask != 0) {
            return (i << 1u) + ((tzcnt(cmp_mask) + 1u) >> 1u);
        }
    }
    // No entries were found
    start = N;
#else // No SSE4.2 support
    while (start < N && !pred(children[start]))
        ++start;
#endif
    return start;
}

template <typename NodePtr, unsigned int N>
[[nodiscard]] inline unsigned int find_first_null(const children_array<NodePtr, N>& children,
                                                  unsigned int i) noexcept
{
    struct is_null_node {
        unsigned int operator()(unsigned int x) const noexcept { return x; }
        bool operator()(NodePtr ptr) const noexcept { return ptr == nullptr; }
    };
    return find_first_eq(children, i, is_null_node());
}

template <typename NodePtr, unsigned int N>
[[nodiscard]] inline unsigned int find_first_non_null(const children_array<NodePtr, N>& children,
                                                      unsigned int i) noexcept
{
    struct is_non_null_node {
        unsigned int operator()(unsigned int x) const noexcept { return x ^ 0xFFFF; }
        bool operator()(NodePtr ptr) const noexcept { return ptr != nullptr; }
    };
    return find_first_eq(children, i, is_non_null_node());
}

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
    using leaf_unique_ptr = typename parent_type::leaf_unique_ptr;
    using const_iterator = typename Db::const_iterator;
    using iterator = typename Db::iterator;

private:
    friend Db;

    using basic_inode_48_parent<Db>::basic_inode_48_parent;

    [[nodiscard]] constexpr iterator populate(unique_node_ptr<inode16_type, Db> source_node,
                                              leaf_unique_ptr child, std::uint8_t key_byte) noexcept
    {
        assert(source_node->is_full());
        assert(this->is_min_size());

        auto* const __restrict__ source_node_ptr = source_node.get();

        // TODO(laurynas): initialize at declaration
        // Cannot use memset without C++20 atomic_ref, but even then check whether
        // this compiles to memset already
        std::fill(child_indices.begin(), child_indices.end(), empty_child);

        // TODO(laurynas): consider AVX512 scatter?
        for (std::uint8_t i = 0; i != inode16_type::capacity; ++i) {
            const std::uint8_t existing_key_byte = source_node_ptr->keys.byte_array[i];
            child_indices[existing_key_byte] = i;
            children[i] = source_node_ptr->children[i];
            this->reparent(children[i], existing_key_byte);
        }

        assert(child_indices[key_byte] == empty_child);
        child_indices[key_byte] = inode16_type::capacity;
        children[inode16_type::capacity] = child.release();
        iterator inserted(children[inode16_type::capacity], key_byte, this->tagged_self());

        std::fill(std::next(children.pointer_array.begin(), inode16_type::capacity + 1),
                  children.pointer_array.end(), nullptr);
        return inserted;
    }

public:
    constexpr basic_inode_48(inode256_type& source_node, std::uint8_t child_to_delete) noexcept
        : parent_type(source_node)
    {
        source_node.children[child_to_delete] = nullptr;

        parent_type::index(*this, child_to_delete);

        std::fill(child_indices.begin(), child_indices.end(), empty_child);

        std::uint8_t next_child = 0;
        for (unsigned child_i = 0; child_i != 256; ++child_i) {
            const auto child_ptr = source_node.children[child_i];
            if (child_ptr == nullptr)
                continue;

            child_indices[child_i] = next_child;
            children[next_child] = source_node.children[child_i];
            this->reparent(children[next_child], child_i);
            ++next_child;

            if (next_child == this->children_count)
                break;
        }
    }

    constexpr iterator add(leaf_unique_ptr child, std::uint8_t key_byte) noexcept
    {
        assert(child_indices[key_byte] == empty_child);
        unsigned int i = find_first_null(children, 0);
        assert(i < children.size());
        assert(children[i] == nullptr);
        child_indices[key_byte] = i;
        children[i] = child.release();
        ++this->children_count;

        return iterator(children[i], key_byte, this->tagged_self());
    }

    [[nodiscard]] constexpr const_iterator leftmost_child(unsigned int start) noexcept
    {
        auto it = std::find_if_not(std::next(child_indices.begin(), start), child_indices.end(),
                                   [](std::uint8_t u) { return u == empty_child; });
        return it != child_indices.end()
                   ? const_iterator(children[*it], it - child_indices.begin(), this->tagged_self())
                   : const_iterator{};
    }

    [[nodiscard]] constexpr std::pair<const_iterator, std::uint8_t> lower_bound(
        std::uint8_t key_byte) noexcept
    {
        auto pos = leftmost_child(key_byte);
        return std::make_pair(pos, pos.index());
    }

    constexpr void remove(std::uint8_t child_index) noexcept
    {
        verify_remove_preconditions(child_index);
        children[child_indices[child_index]] = nullptr;
        assign_empty_child(child_indices, child_index);
        --this->children_count;
    }

    [[nodiscard]] constexpr const_iterator find_child(std::uint8_t key_byte) noexcept
    {
        const auto child_i = child_indices[key_byte];
        return child_i != empty_child
                   ? const_iterator(children[child_i], key_byte, this->tagged_self())
                   : const_iterator{};
    }

    constexpr void replace(iterator pos, node_ptr child) noexcept
    {
        assert(pos.parent() == this->tagged_self());
        const auto child_i = child_indices[pos.index()];
        assert(pos.node() == children[child_i]);
        children[child_i] = child;
        this->reparent(children[child_i], pos.index());
    }

    constexpr void delete_subtree(Db& db_instance) noexcept
    {
        for (unsigned i = 0; i < this->capacity; ++i) {
            const auto child = children[i];
            if (child != nullptr) {
                db_instance.deallocate(child);
            }
        }
    }

    [[gnu::cold, gnu::noinline]] void dump(std::ostream& os) const
    {
        parent_type::dump(os);
        os << ", key bytes & child indices\n";
        for (unsigned i = 0; i != 256; ++i)
            if (child_indices[i] != empty_child) {
                os << " ";
                dump_byte(os, static_cast<std::uint8_t>(i));
                os << ", child index = " << static_cast<unsigned>(child_indices[i]) << ": ";
                assert(children[child_indices[i]] != nullptr);
                parent_type::dump(os, children[child_indices[i]]);
            }
    }

private:
    constexpr void verify_remove_preconditions(std::uint8_t child_index) const noexcept
    {
        assert(child_indices[child_index] != empty_child);
        assert(children[child_indices[child_index]] != nullptr);
        boost::ignore_unused(child_index);
    }

    std::array<std::uint8_t, 256> child_indices;
    children_array<node_ptr, basic_inode_48::capacity> children;

    friend basic_inode_16<Db>;
    friend basic_inode_256<Db>;
};

template <typename Db>
using basic_inode_256_parent =
    basic_inode<Db, 49, 256, node_type::I256, basic_inode_48<Db>, fake_inode, basic_inode_256<Db>>;

template <typename Db> class basic_inode_256 : public basic_inode_256_parent<Db>
{
    using parent_type = basic_inode_256_parent<Db>;
    using inode48_type = typename parent_type::inode48_type;
    using inode256_type = typename parent_type::inode256_type;
    using node_ptr = typename parent_type::node_ptr;
    using leaf_unique_ptr = typename parent_type::leaf_unique_ptr;
    using const_iterator = typename Db::const_iterator;
    using iterator = typename Db::iterator;

private:
    friend Db;

    using basic_inode_256_parent<Db>::basic_inode_256_parent;

    [[nodiscard]] constexpr iterator populate(unique_node_ptr<inode48_type, Db> source_node,
                                              leaf_unique_ptr child, std::uint8_t key_byte) noexcept
    {
        assert(source_node->is_full());
        assert(this->is_min_size());

        unsigned children_copied = 0;
        unsigned i = 0;
        while (true) {
            const auto children_i = source_node->child_indices[i];
            if (children_i == empty_child) {
                children[i] = nullptr;
            } else {
                children[i] = source_node->children[children_i];
                this->reparent(children[i], i);
                ++children_copied;
                if (children_copied == inode48_type::capacity)
                    break;
            }
            ++i;
        }

        ++i;
        for (; i < basic_inode_256::capacity; ++i)
            children[i] = nullptr;

        assert(children[key_byte] == nullptr);
        children[key_byte] = child.release();

        return iterator(children[key_byte], key_byte, this->tagged_self());
    }

public:
    [[nodiscard]] constexpr iterator add(leaf_unique_ptr child, std::uint8_t key_byte) noexcept
    {
        assert(children[key_byte] == nullptr);
        children[key_byte] = child.release();
        ++this->children_count;

        return iterator(children[key_byte], key_byte, this->tagged_self());
    }

    constexpr void remove(std::uint8_t child_index) noexcept
    {
        assert(children[child_index] != nullptr);
        children[child_index] = nullptr;
        --this->children_count;
    }

    [[nodiscard]] constexpr const_iterator find_child(std::uint8_t key_byte) noexcept
    {
        return const_iterator(children[key_byte], key_byte, this->tagged_self());
    }

    [[nodiscard]] constexpr const_iterator leftmost_child(unsigned int key_byte) noexcept
    {
        const unsigned int i = find_first_non_null(children, key_byte);
        return i < children.size() ? const_iterator(children[i], i, this->tagged_self())
                                   : const_iterator{};
    }

    [[nodiscard]] constexpr std::pair<const_iterator, std::uint8_t> lower_bound(
        std::uint8_t key_byte) noexcept
    {
        auto pos = leftmost_child(key_byte);
        return std::make_pair(pos, pos.index());
    }

    constexpr void replace(iterator pos, node_ptr child) noexcept
    {
        const std::uint8_t key_byte = pos.index();
        assert(pos.parent() == this->tagged_self() && pos.node() == children[key_byte]);
        children[key_byte] = child;
        this->reparent(child, key_byte);
    }

    template <typename Function>
    constexpr void for_each_child(Function func) const noexcept(noexcept(func(0, node_ptr{})))
    {
        for (unsigned i = 0; i != 256; ++i) {
            const auto child_ptr = children[i];
            if (child_ptr != nullptr) {
                func(i, child_ptr);
            }
        }
    }

    constexpr void delete_subtree(Db& db_instance) noexcept
    {
        for_each_child(
            [&db_instance](unsigned, node_ptr child) noexcept { db_instance.deallocate(child); });
    }

    [[gnu::cold, gnu::noinline]] void dump(std::ostream& os) const
    {
        parent_type::dump(os);
        os << ", key bytes & children:\n";
        for_each_child([&os](unsigned i, node_ptr child) {
            os << ' ';
            dump_byte(os, static_cast<std::uint8_t>(i));
            os << ' ';
            parent_type::dump(os, child);
        });
    }

private:
    children_array<node_ptr, basic_inode_256::capacity> children;

    friend inode48_type;
};

} // namespace detail
} // namespace art

#endif // ART_DETAIL_ART_NODES_HEADER_INCLUDED
