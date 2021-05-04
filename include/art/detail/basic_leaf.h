#ifndef ART_DETAIL_BASIC_LEAF_HEADER_INCLUDED
#define ART_DETAIL_BASIC_LEAF_HEADER_INCLUDED

#include "art_node_base.h"

#include <memory>
#include <ostream>
#include <type_traits>

namespace art
{
namespace detail
{

// Helper struct for leaf node-related data
template <typename Header, typename T, typename Alloc>
struct basic_leaf final : public art_node_base<Header> {
    using value_type = T;
    using allocator_type = Alloc;
    using allocator_traits = std::allocator_traits<Alloc>;
    using bitwise_key = typename Header::bitwise_key;

    using base_t = art_node_base<Header>;

    explicit constexpr basic_leaf(bitwise_key key) noexcept
        : base_t(node_type::LEAF, key)
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
        base_t::dump(os);
        os << ", value: " << *addr() << '\n';
    }

private:
    T* addr() noexcept { return reinterpret_cast<T*>(&data); }
    const T* addr() const noexcept { return reinterpret_cast<const T*>(&data); }

private:
    std::aligned_storage_t<sizeof(T), alignof(T)> data;
};

// Specialization for integral constants that do not take any space
template <typename Header, typename T, T V, typename Alloc>
struct basic_leaf<Header, std::integral_constant<T, V>, Alloc> final
    : public art_node_base<Header> {
    using value_type = T;
    using allocator_type = Alloc;
    using bitwise_key = typename Header::bitwise_key;

    using base_t = art_node_base<Header>;

    explicit constexpr basic_leaf(bitwise_key key) noexcept
        : base_t(node_type::LEAF, key)
    {
    }

    // Does nothing, since the value is a constant expression
    template <typename... Args> static void emplace_value(allocator_type&, Args&&...) noexcept {}
    static void destroy_value(allocator_type&) noexcept {}

    void dump(std::ostream& os) const
    {
        base_t::dump(os);
        os << '\n';
    }
};

} // namespace detail
} // namespace art

#endif // ART_DETAIL_BASIC_LEAF_HEADER_INCLUDED
