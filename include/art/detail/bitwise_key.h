#ifndef ART_DETAIL_BITWISE_KEY_HEADER_INCLUDED
#define ART_DETAIL_BITWISE_KEY_HEADER_INCLUDED

#include <boost/endian/conversion.hpp>
#include <boost/integer.hpp>

#include <cassert>
#include <cstdint>
#include <functional>

namespace art
{
namespace detail
{

namespace comparison_ops
{

struct less_tag {
};

struct greater_tag {
};

// Internal ART key in binary-comparable format
template <typename T> struct compared_arg;

// Specializations for particular orderings
template <typename T> struct compared_arg<std::less<T>> {
    using argument_type = T;
    using order = less_tag;
};

template <typename T> struct compared_arg<std::greater<T>> {
    using argument_type = T;
    using order = greater_tag;
};

} // namespace comparison_ops

template <typename T> struct bitwise_compare;

// Specializations for particular orderings
template <> struct bitwise_compare<comparison_ops::less_tag> {
    template <typename T> [[nodiscard]] inline static constexpr T byte_swap(T k) noexcept
    {
        // Flip bytes to Big Endian order (no-op on Big Endian architectures)
        return boost::endian::native_to_big(k);
    }
};

template <> struct bitwise_compare<comparison_ops::greater_tag> {
    template <typename T> [[nodiscard]] inline static constexpr T byte_swap(T k) noexcept
    {
        // Flip bytes to Little Endian order (no-op on Little Endian architectures)
        return boost::endian::native_to_little(k);
    }
};

template <typename Ptr, typename Order> struct ptr_bitwise_compare {
    static_assert(std::is_pointer<Ptr>::value, "Unsupported pointer type");

    using compare_t = bitwise_compare<Order>;
    inline static constexpr std::uintptr_t byte_swap(Ptr k) noexcept
    {
        return compare_t::byte_swap(reinterpret_cast<std::uintptr_t>(k));
    }
};

template <typename Int, typename UInt, typename Order> struct int_bitwise_compare {
    static_assert(std::is_signed<Int>::value && !std::is_floating_point<Int>::value,
                  "Unsupported signed integer type");

    using compare_t = bitwise_compare<Order>;
    inline static constexpr UInt byte_swap(Int k) noexcept
    {
        return compare_t::byte_swap(static_cast<UInt>(-k));
    }
};

template <typename T, typename Key, typename Policy> struct unsigned_integral_bitwise_key {
    static_assert(std::is_unsigned<Key>::value && std::is_integral<Key>::value,
                  "Unsupported unsigned integral key type");
    static_assert(sizeof(T) == sizeof(Key), "Invalid key size");

    using size_type = unsigned int;
    static constexpr size_type num_bytes = sizeof(Key);

    explicit constexpr unsigned_integral_bitwise_key(T k) noexcept
        : key{Policy::byte_swap(k)}
    {
    }

    [[nodiscard]] bool match(unsigned_integral_bitwise_key other) const noexcept
    {
        return key.bitkey == other.key.bitkey;
    }

    // Internal nodes use partial keys
    [[nodiscard]] size_type size() const noexcept { return key.bytes[num_bytes - 1]; }
    [[nodiscard]] std::uint8_t operator[](size_type index) const noexcept
    {
        assert(index + 1 < num_bytes);
        return key.bytes[index];
    }

    [[nodiscard]] std::uint8_t front() const noexcept { return key.bytes[0]; }

    void shift_right(size_type nbytes) noexcept
    {
        const std::uint8_t siz = size();
        assert(nbytes <= siz);
        key.bitkey >>= (nbytes * CHAR_BIT);
        key.bytes[num_bytes - 1] = siz - nbytes;
    }

private:
    union {
        Key bitkey;
        std::array<std::uint8_t, num_bytes> bytes;
    } key;
};

// Support for unsigned keys
template <typename Key, typename Order> struct unsigned_bitwise_key {
    using type = unsigned_integral_bitwise_key<Key, Key, bitwise_compare<Order>>;
};

template <typename Key, typename Order>
using unsigned_bitwise_key_t = typename unsigned_bitwise_key<Key, Order>::type;

// Supports for non member-function pointers
template <typename Key, typename Order> struct pointer_bitwise_key {
    using type =
        unsigned_integral_bitwise_key<Key, std::uintptr_t, ptr_bitwise_compare<Key, Order>>;
};

template <typename Key, typename Order>
using pointer_bitwise_key_t = typename pointer_bitwise_key<Key, Order>::type;

// Supports for signed integers of various sizes. Floats and doubles are
// explicitly not supported
template <typename Key, typename Order> struct signed_bitwise_key {
    using uint_t = typename boost::uint_t<sizeof(Key) * CHAR_BIT>::fast;
    using type =
        unsigned_integral_bitwise_key<Key, uint_t, int_bitwise_compare<Key, uint_t, Order>>;
};

template <typename Key, typename Order>
using signed_bitwise_key_t = typename signed_bitwise_key<Key, Order>::type;

template <typename Key, typename Compare> struct bitwise_key_compare {
    using predicate = comparison_ops::compared_arg<Compare>;

    using arg_t = typename predicate::argument_type;
    static_assert(std::is_convertible<Key, arg_t>::value, "Incompatible comparison predicate");

    using order = typename predicate::order;
    using type = std::conditional_t<
        std::is_signed<arg_t>::value, signed_bitwise_key_t<arg_t, order>,
        std::conditional_t<std::is_pointer<arg_t>::value, pointer_bitwise_key_t<arg_t, order>,
                           unsigned_bitwise_key_t<arg_t, order>>>;
};

template <typename Key, typename Compare>
using bitwise_key_t = typename bitwise_key_compare<Key, Compare>::type;

} // namespace detail
} // namespace art

#endif // ART_DETAIL_BITWISE_KEY_HEADER_INCLUDED