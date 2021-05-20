#ifndef ART_FAST_CONST_ARGUMENT_HEADER_INCLUDED
#define ART_FAST_CONST_ARGUMENT_HEADER_INCLUDED

#include <type_traits>

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

} // namespace detail
} // namespace art

#endif // ART_FAST_CONST_ARGUMENT_HEADER_INCLUDED
