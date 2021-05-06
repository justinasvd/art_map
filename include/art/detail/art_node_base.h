#ifndef ART_DETAIL_ART_NODE_BASE_HEADER_INCLUDED
#define ART_DETAIL_ART_NODE_BASE_HEADER_INCLUDED

#include "dump_byte.h"

#include <cassert>
#include <cstdint>

namespace art
{
namespace detail
{

enum class node_type : std::uint8_t { LEAF, I4, I16, I48, I256 };

// Common base for various node types and leaves. This common base ensures that
// common functionality could be achieved though the base pointer.
template <typename Header> struct art_node_base {
    using bitwise_key = typename Header::bitwise_key;
    using key_size_type = typename bitwise_key::size_type;

    constexpr art_node_base(node_type type, bitwise_key key, key_size_type key_size,
                            art_node_base* parent) noexcept
        : parent_(parent)
        , header(type, key, key_size)
    {
    }

    [[nodiscard]] node_type type() const noexcept { return header.type(); }

    [[nodiscard]] art_node_base* parent() const noexcept { return parent_; }
    constexpr void reparent(art_node_base* parent) noexcept
    {
        assert(parent && parent->type() != node_type::LEAF);
        parent_ = parent;
    }

    // This is meaningful for internal nodes only
    [[nodiscard]] constexpr key_size_type prefix_length() const noexcept
    {
        return header.prefix_length();
    }
    [[nodiscard]] constexpr key_size_type shared_prefix_length(bitwise_key key) const noexcept
    {
        return header.shared_prefix_length(key);
    }
    [[nodiscard]] constexpr std::pair<bitwise_key, key_size_type> shared_prefix(
        bitwise_key key, key_size_type size) const noexcept
    {
        return header.shared_prefix(key, size);
    }

    // This is meaningful for leaves only
    [[nodiscard]] bool match(bitwise_key key) const noexcept { return header.match(key); }

    // Prefix manipulation routines
    constexpr void shift_right(key_size_type size) noexcept { header.shift_right(size); }
    [[nodiscard]] constexpr std::uint8_t pop_front() noexcept { return header.pop_front(); }

    void dump(std::ostream& os) const
    {
        switch (type()) {
        case node_type::LEAF:
            os << "LEAF:";
            break;
        case node_type::I4:
            os << "I4:";
            break;
        case node_type::I16:
            os << "I16:";
            break;
        case node_type::I48:
            os << "I48:";
            break;
        case node_type::I256:
            os << "I256:";
            break;
        }

        // Dump bitwise key prefix
        const key_size_type len = prefix_length();
        os << " key prefix len = " << len;
        if (len) {
            os << ", key prefix =";
            for (key_size_type i = 0; i != len; ++i)
                dump_byte(os, header[i]);
        }
    }

private:
    art_node_base* parent_;
    Header header;
};

} // namespace detail
} // namespace art

#endif // ART_DETAIL_ART_NODE_BASE_HEADER_INCLUDED
