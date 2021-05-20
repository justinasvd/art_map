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

    constexpr art_node_base(node_type type, bitwise_key key) noexcept
        : header(key)
        , type_(type)
    {
    }

    constexpr art_node_base(node_type type, bitwise_key key, key_size_type key_size) noexcept
        : header(key, key_size)
        , type_(type)
    {
    }

    constexpr art_node_base(node_type type,
                            const std::pair<bitwise_key, key_size_type>& prefix) noexcept
        : art_node_base(type, prefix.first, prefix.second)
    {
    }

    [[nodiscard]] constexpr node_type type() const noexcept { return type_; }

    [[nodiscard]] constexpr std::uint8_t front() const noexcept { return header.front(); }
    [[nodiscard]] constexpr bitwise_key key() const noexcept { return header.key(); }

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

    // Prefix manipulation routines
    constexpr void shift_right(key_size_type size) noexcept { header.shift_right(size); }
    constexpr void push_front(std::uint8_t key) noexcept { header.push_front(key); }
    constexpr void push_front(const std::pair<bitwise_key, key_size_type>& prefix) noexcept
    {
        header.push_front(prefix.first, prefix.second);
    }

    void dump(std::ostream& os) const
    {
        switch (type()) {
        case node_type::LEAF:
            os << "LEAF: key =";
            dump(os, key());
            break;
        case node_type::I4:
            os << "I4:";
            dump(os, prefix());
            break;
        case node_type::I16:
            os << "I16:";
            dump(os, prefix());
            break;
        case node_type::I48:
            os << "I48:";
            dump(os, prefix());
            break;
        default:
            assert(type() == node_type::I256);
            os << "I256:";
            dump(os, prefix());
            break;
        }
    }

    // Dump bitwise key prefix
    static void dump(std::ostream& os, bitwise_key key) { dump(os, key, key.max_size()); }
    static void dump(std::ostream& os, const std::pair<bitwise_key, key_size_type>& prefix)
    {
        os << " key prefix len = " << static_cast<std::size_t>(prefix.second);
        if (prefix.second) {
            os << ", key prefix =";
            dump(os, prefix.first, prefix.second);
        }
    }

private:
    static void dump(std::ostream& os, bitwise_key key, key_size_type len)
    {
        for (key_size_type i = 0; i != len; ++i)
            dump_byte(os, key[i]);
    }

private:
    Header header;
    const node_type type_;
};

} // namespace detail
} // namespace art

#endif // ART_DETAIL_ART_NODE_BASE_HEADER_INCLUDED
