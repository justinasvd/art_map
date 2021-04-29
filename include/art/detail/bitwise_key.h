#ifndef ART_DETAIL_BITWISE_KEY_HEADER_INCLUDED
#define ART_DETAIL_BITWISE_KEY_HEADER_INCLUDED

#include <boost/endian/conversion.hpp>
#include <boost/integer.hpp>

#include <functional>

namespace art
{
namespace detail
{

namespace comparison_ops
{

// Internal ART key in binary-comparable format
template <typename T> struct compared_arg;

// Specializations for particular orderings
template <typename T> struct compared_arg<std::less<T>> {
    using argument_type = T;
    using order = std::true_type;
};

template <typename T> struct compared_arg<std::greater<T>> {
    using argument_type = T;
    using order = std::false_type;
};

} // namespace comparison_ops

template <typename T> struct bitwise_compare;

// Specializations for particular orderings
template <> struct bitwise_compare<std::true_type> {
    template <typename T> [[nodiscard]] inline static constexpr T byte_swap(T k) noexcept
    {
        // Flip bytes to Big Endian order (no-op on Big Endian architectures)
        return boost::endian::native_to_big(k);
    }
};

template <> struct bitwise_compare<std::false_type> {
    template <typename T> [[nodiscard]] inline static constexpr T byte_swap(T k) noexcept
    {
        // Flip bytes to Little Endian order (no-op on Little Endian architectures)
        return boost::endian::native_to_little(k);
    }
};

template <typename Key> struct unsigned_integral_bitwise_key {
    static_assert(std::is_unsigned<Key>::value && std::is_integral<Key>::value,
                  "Unsupported unsigned integral key type");

    static constexpr std::size_t num_bytes = sizeof(Key);

    // Full bits may be used iff the root node is also a leaf,
    // otherwise only partial keys are used
    [[nodiscard]] Key full_key() const noexcept { return key.bitkey; }

    // Nodes use partial keys
    [[nodiscard]] std::uint8_t size() const noexcept { return key.bytes[num_bytes - 1]; }
    [[nodiscard]] std::uint8_t operator[](std::size_t index) const noexcept
    {
        assert(index + 1 < num_bytes);
        return key.bytes[index];
    }

    void shift_right(std::size_t nbytes) noexcept
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

template <typename Key, typename Order> struct unsigned_bitwise_key {
    static_assert(std::is_unsigned<Key>::value && std::is_integral<Key>::value,
                  "Unsupported unsigned key type");

    using bit_compare = bitwise_compare<Order>;

    static constexpr std::size_t max_size = sizeof(Key);
    static constexpr std::size_t size() noexcept { return max_size; }

    constexpr unsigned_bitwise_key(Key key) noexcept
        : k(bit_compare::byte_swap(key))
    {
    }

    [[nodiscard]] constexpr std::uint8_t operator[](std::size_t index) const noexcept
    {
        assert(index < size());
        // cppcheck-suppress objectIndex
        return (reinterpret_cast<const std::uint8_t*>(&k))[index];
    }

    constexpr void shift_right(const std::size_t num_bytes) noexcept
    {
        k >>= (num_bytes * CHAR_BIT);
    }

    Key to_key() const noexcept { return k; }

private:
    Key k;
};

template <typename Key, typename Order> struct pointer_bitwise_key {
    static_assert(std::is_pointer<Key>::value, "Unsupported pointer key type");

    using bit_compare = bitwise_compare<Order>;

    static constexpr std::size_t max_size = sizeof(Key);
    static constexpr std::size_t size() noexcept { return max_size; }

    explicit constexpr pointer_bitwise_key(Key key) noexcept
        : k(bit_compare::byte_swap(key))
    {
    }

    [[nodiscard]] constexpr std::uint8_t operator[](std::size_t index) const noexcept
    {
        assert(index < size());
        // cppcheck-suppress objectIndex
        return (reinterpret_cast<const std::uint8_t*>(&k))[index];
    }

    constexpr void shift_right(const std::size_t num_bytes) noexcept
    {
        k >>= (num_bytes * CHAR_BIT);
    }

    Key to_key() const noexcept { return k; }

private:
    Key k;
};

// Supports only signed integers of various sizes. Floats and doubles are
// explicitly not supported
template <typename Key, typename Order> struct signed_bitwise_key {
    static_assert(std::is_signed<Key>::value && !std::is_floating_point<Key>::value,
                  "Unsupported signed key type");

    using bit_compare = bitwise_compare<Order>;
    using uint_t = typename boost::uint_t<sizeof(Key) * CHAR_BIT>::fast;

    static constexpr std::size_t max_size = sizeof(uint_t);
    static constexpr std::size_t size() noexcept { return max_size; }

    constexpr signed_bitwise_key(Key key) noexcept
        : k(bit_compare::byte_swap(static_cast<uint_t>(key)))
    {
    }

private:
    uint_t k;
};

template <typename Key, typename Compare> struct bitwise_key_compare {
    using predicate = comparison_ops::compared_arg<Compare>;

    using arg_t = typename predicate::argument_type;
    static_assert(std::is_convertible<Key, arg_t>::value, "Incompatible comparison predicate");

    using order = typename predicate::order;
    using type = std::conditional_t<
        std::is_signed<arg_t>::value, signed_bitwise_key<arg_t, order>,
        std::conditional_t<std::is_pointer<arg_t>::value, pointer_bitwise_key<arg_t, order>,
                           unsigned_bitwise_key<arg_t, order>>>;
};

template <typename Key, typename Compare>
using bitwise_key_t = typename bitwise_key_compare<Key, Compare>::type;

} // namespace detail
} // namespace art

#endif // ART_DETAIL_BITWISE_KEY_HEADER_INCLUDED
