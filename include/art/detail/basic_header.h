#ifndef ART_DETAIL_BASIC_HEADER_HEADER_INCLUDED
#define ART_DETAIL_BASIC_HEADER_HEADER_INCLUDED

#include "art_node_base.h"
#include "dump_byte.h"

#include <ostream>
#include <type_traits>

namespace art
{
namespace detail
{

// A common prefix shared by all node types
template <typename BitwiseKey> struct basic_header final {
    using bitwise_key = BitwiseKey;
    using key_size_type = typename BitwiseKey::size_type;

    constexpr basic_header(node_type type, bitwise_key key) noexcept
        : type_(type)
        , key_(key)
    {
    }

    [[nodiscard]] constexpr node_type type() const noexcept { return type_; }

    [[nodiscard]] bool match(bitwise_key other) const noexcept { return key_.match(other); }

    [[nodiscard]] constexpr key_size_type prefix_length() const noexcept
    {
        // const unsigned result = key_.prefix_length();
        // assert(result <= key_prefix_capacity);
        // return result;
        return 0;
    }
    [[nodiscard]] constexpr key_size_type shared_prefix_length(bitwise_key key) const noexcept
    {
        /*const auto prefix_u64 = header_as_uint64() >> 8U;
    return shared_len(static_cast<std::uint64_t>(shifted_key.to_key()), prefix_u64,
                      header.key_prefix_length());*/
    }

    void dump(std::ostream& os) const
    {
        switch (type_) {
        case node_type::LEAF:
            os << "LEAF: ";
            dump_key_prefix(os, key_.max_size());
            break;
        case node_type::I4:
            os << "I4: ";
            dump_key_prefix(os);
            break;
        case node_type::I16:
            os << "I16: ";
            dump_key_prefix(os);
            break;
        case node_type::I48:
            os << "I48: ";
            dump_key_prefix(os);
            break;
        case node_type::I256:
            os << "I256: ";
            dump_key_prefix(os);
            break;
        }
    }

private:
    // Dump bitwise key (or its prefix)
    void dump_key_prefix(std::ostream& os, key_size_type len)
    {
        os << ", key prefix len = " << len;
        if (len > 0) {
            os << ", key prefix =";
            for (key_size_type i = 0; i != len; ++i)
                dump_byte(os, key_[i]);
        }
    }

    void dump_key_prefix(std::ostream& os) const { dump_key_prefix(os, prefix_length()); }

private:
    node_type type_;
    bitwise_key key_;
};

} // namespace detail
} // namespace art

#endif // ART_DETAIL_BASIC_HEADER_HEADER_INCLUDED
