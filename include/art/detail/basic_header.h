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

    [[nodiscard]] constexpr bool match(bitwise_key other) const noexcept
    {
        return key_.match(other);
    }

    [[nodiscard]] std::uint8_t operator[](key_size_type index) const noexcept
    {
        return key_[index];
    }

    [[nodiscard]] constexpr key_size_type prefix_length() const noexcept
    {
        assert(size_ <= key_.max_size());
        return size_;
    }
    [[nodiscard]] constexpr key_size_type shared_prefix_length(bitwise_key key) const noexcept
    {
        return bitwise_key::shared_len(key, key_, size_);
    }
    [[nodiscard]] constexpr std::pair<bitwise_key, key_size_type> shared_prefix(
        bitwise_key key, key_size_type size) const noexcept
    {
        assert(size <= size_);
        const key_size_type shared_size = bitwise_key::shared_len(key, key_, size);
        return std::make_pair(bitwise_key::partial_key(key_, shared_size), shared_size);
    }

    constexpr void shift_right(key_size_type size) noexcept
    {
        assert(size <= size_);
        key_.shift_right(size);
        size_ -= size;
    }

    [[nodiscard]] constexpr std::uint8_t pop_front() noexcept
    {
        const std::uint8_t front = key_.front();
        shift_right(1);
        return front;
    }

private:
    node_type type_;
    key_size_type size_;
    bitwise_key key_;
};

} // namespace detail
} // namespace art

#endif // ART_DETAIL_BASIC_HEADER_HEADER_INCLUDED
