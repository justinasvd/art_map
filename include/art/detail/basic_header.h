#ifndef ART_DETAIL_BASIC_HEADER_HEADER_INCLUDED
#define ART_DETAIL_BASIC_HEADER_HEADER_INCLUDED

#include "node_type.h"

#include <type_traits>

namespace art
{
namespace detail
{

// A common prefix shared by all node types
template <typename BitwiseKey> struct basic_header final {
    using bitwise_key = BitwiseKey;

    constexpr basic_header(node_type type, bitwise_key key) noexcept
        : type_(type)
        , key_(key)
    {
    }

    [[nodiscard]] constexpr node_type type() const noexcept { return type_; }

private:
    node_type type_;
    bitwise_key key_;
};

// A common prefix shared by all node types
struct default_header final {
    explicit constexpr default_header(node_type type_) noexcept
        : m_type{type_}
    {
    }

    [[nodiscard]] constexpr node_type type() const noexcept { return m_type; }

private:
    const node_type m_type;
};

} // namespace detail
} // namespace art

#endif // ART_DETAIL_BASIC_HEADER_HEADER_INCLUDED
