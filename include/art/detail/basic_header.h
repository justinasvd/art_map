#ifndef ART_DETAIL_BASIC_HEADER_HEADER_INCLUDED
#define ART_DETAIL_BASIC_HEADER_HEADER_INCLUDED

#include "art_node_base.h"

#include <type_traits>

namespace art
{
namespace detail
{

// A common prefix shared by all node types
template <typename BitwiseKey> struct basic_header final {
    using bitwise_key = BitwiseKey;
    using key_size_type = typename BitwiseKey::size_type;

    constexpr basic_header(node_type type, bitwise_key key, key_size_type key_size) noexcept
        : type_(type)
        , size_(key_size)
        , key_(key)
    {
    }

    [[nodiscard]] constexpr node_type type() const noexcept { return type_; }

    [[nodiscard]] constexpr bitwise_key key() const noexcept { return key_; }
    [[nodiscard]] constexpr key_size_type size() const noexcept
    {
        assert(size_ <= key_.max_size());
        return size_;
    }

    constexpr void shift_right(key_size_type size) noexcept
    {
        assert(size <= size_);
        key_.shift_right(size);
        size_ -= size;
    }

private:
    node_type type_;
    key_size_type size_;
    bitwise_key key_;
};

} // namespace detail
} // namespace art

#endif // ART_DETAIL_BASIC_HEADER_HEADER_INCLUDED
