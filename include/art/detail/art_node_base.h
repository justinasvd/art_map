#ifndef ART_DETAIL_ART_NODE_BASE_HEADER_INCLUDED
#define ART_DETAIL_ART_NODE_BASE_HEADER_INCLUDED

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
        : header(type, key)
    {
    }

    [[nodiscard]] node_type type() const noexcept { return header.type(); }

    // This is meaningful for internal nodes only
    [[nodiscard]] constexpr key_size_type prefix_length() const noexcept
    {
        return header.prefix_length();
    }
    [[nodiscard]] constexpr key_size_type shared_prefix_length(bitwise_key key) const noexcept
    {
        return header.shared_prefix_length(key);
    }

    // This is meaningful for leaves only
    [[nodiscard]] bool match(bitwise_key key) const noexcept { return header.match(key); }

private:
    Header header;
};

} // namespace detail
} // namespace art

#endif // ART_DETAIL_ART_NODE_BASE_HEADER_INCLUDED
