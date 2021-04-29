#ifndef ART_DETAIL_BASIC_LEAF_HEADER_INCLUDED
#define ART_DETAIL_BASIC_LEAF_HEADER_INCLUDED

#include "node_type.h"

#include <memory>
#include <ostream>
#include <type_traits>

namespace art
{
namespace detail
{

// Helper struct for leaf node-related data
template <typename Header, typename T, typename Alloc> struct basic_leaf final {
    using value_type = T;
    using allocator_type = Alloc;
    using allocator_traits = std::allocator_traits<Alloc>;

    explicit constexpr basic_leaf(typename Header::bitwise_key key) noexcept
        : header(node_type::LEAF, key)
    {
    }

    template <typename... Args>
    void emplace_value(allocator_type& alloc,
                       Args&&... args) noexcept(std::is_nothrow_constructible<T>::value)
    {
        allocator_traits::construct(alloc, addr(), std::forward<Args>(args)...);
    }

    void destroy_value(allocator_type& alloc) noexcept(std::is_nothrow_destructible<T>::value)
    {
        allocator_traits::destroy(alloc, addr());
    }

    void dump(std::ostream& os) const
    {
        os << "LEAF: ";
        header.dump(os);
        os << ", value: " << *addr() << '\n';
    }

private:
    T* addr() noexcept { return reinterpret_cast<T*>(&data); }
    const T* addr() const noexcept { return reinterpret_cast<const T*>(&data); }

private:
    const Header header;
    std::aligned_storage_t<sizeof(T), alignof(T)> data;
};

// Specialization for integral constants that do not take any space
template <typename Header, typename T, T V, typename Alloc>
struct basic_leaf<Header, std::integral_constant<T, V>, Alloc> final {
    using value_type = T;
    using allocator_type = Alloc;

    explicit constexpr basic_leaf(typename Header::bitwise_key key) noexcept
        : header(node_type::LEAF, key)
    {
    }

    // Does nothing, since the value is a constant expression
    template <typename... Args> static void emplace_value(allocator_type&, Args&&...) noexcept {}
    static void destroy_value(allocator_type&) noexcept {}

    void dump(std::ostream& os) const
    {
        os << "LEAF: ";
        header.dump(os);
        os << '\n';
    }

private:
    const Header header;
};

} // namespace detail
} // namespace art

#endif // ART_DETAIL_BASIC_LEAF_HEADER_INCLUDED
