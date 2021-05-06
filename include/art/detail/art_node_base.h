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

    constexpr art_node_base(node_type type, const std::pair<bitwise_key, key_size_type>& prefix,
                            art_node_base* parent) noexcept
        : art_node_base(type, prefix.first, prefix.second, parent)
    {
    }

    [[nodiscard]] node_type type() const noexcept { return header.type(); }

    [[nodiscard]] art_node_base* parent() const noexcept { return parent_; }
    constexpr void reparent(art_node_base* parent) noexcept
    {
        assert(parent && parent->type() != node_type::LEAF);
        parent_ = parent;
    }

    [[nodiscard]] constexpr std::pair<bitwise_key, key_size_type> prefix() const noexcept
    {
        return std::make_pair(header.key(), header.size());
    }

    // This is meaningful for internal nodes only
    [[nodiscard]] constexpr key_size_type prefix_length() const noexcept { return header.size(); }
    [[nodiscard]] constexpr key_size_type shared_prefix_length(bitwise_key key,
                                                               key_size_type size) const noexcept
    {
        return bitwise_key::shared_len(key, header.key(), size);
    }
    [[nodiscard]] constexpr key_size_type shared_prefix_length(bitwise_key key) const noexcept
    {
        return shared_prefix_length(key, header.size());
    }
    [[nodiscard]] std::pair<bitwise_key, key_size_type> shared_prefix(
        bitwise_key key, key_size_type size) const noexcept
    {
        const key_size_type shared_size = shared_prefix_length(key, size);
        return std::make_pair(bitwise_key::partial_key(key, shared_size), shared_size);
    }
    [[nodiscard]] std::pair<bitwise_key, key_size_type> shared_prefix(
        bitwise_key key) const noexcept
    {
        return shared_prefix(key, header.size());
    }

    // This is meaningful for leaves only
    [[nodiscard]] bool match(bitwise_key key) const noexcept { return key.match(header.key()); }

    // Prefix manipulation routines
    constexpr void shift_right(key_size_type size) noexcept { header.shift_right(size); }
    [[nodiscard]] constexpr std::uint8_t pop_front() noexcept
    {
        const std::uint8_t front = header.key().front();
        header.shift_right(1);
        return front;
    }

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
            const auto key = header.key();
            os << ", key prefix =";
            for (key_size_type i = 0; i != len; ++i)
                dump_byte(os, key[i]);
        }
    }

private:
    art_node_base* parent_;
    Header header;
};

} // namespace detail
} // namespace art

#endif // ART_DETAIL_ART_NODE_BASE_HEADER_INCLUDED
