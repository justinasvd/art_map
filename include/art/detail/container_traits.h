#ifndef ART_CONTAINER_TRAITS_HEADER_INCLUDED
#define ART_CONTAINER_TRAITS_HEADER_INCLUDED

#include "basic_header.h"
#include "bitwise_key.h"

namespace art
{
namespace detail
{

// Fast argument type
template <typename T> struct fast_const_argument {
    using const_reference = std::add_lvalue_reference_t<std::add_const_t<T>>;

    // Sufficiently small trivially copyable types are passed by value
    using type = std::conditional_t<std::is_trivially_copyable<T>::value &&
                                        sizeof(T) <= sizeof(const_reference),
                                    T, const_reference>;
};

template <typename T> using fast_const_argument_t = typename fast_const_argument<T>::type;

template <typename Key, typename Value, typename Compare, typename Alloc, typename MultiMap>
struct container_traits {
    using key_type = Key;
    using value_type = Value;
    using pointer = value_type*;
    using const_pointer = const value_type*;
    using reference = value_type&;
    using const_reference = const value_type&;
    using size_type = std::size_t;
    using difference_type = std::ptrdiff_t;
    using key_compare = Compare;
    using allocator_type = Alloc;
    using multi_container = MultiMap;

    // We are computing the bitwise key and friends early, so that in the
    // case of failure, when the compiler says that florbicators must vesterimuzite,
    // the spewed error messages would be at least potentially parsable.
    using bitwise_key = bitwise_key_t<Key, Compare>;
    using header_type = basic_header<bitwise_key>;
    using node_base = art_node_base<header_type>;
};

} // namespace detail
} // namespace art

#endif // ART_CONTAINER_TRAITS_HEADER_INCLUDED
