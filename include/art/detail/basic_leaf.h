#ifndef ART_DETAIL_BASIC_LEAF_HEADER_INCLUDED
#define ART_DETAIL_BASIC_LEAF_HEADER_INCLUDED

#include "art_node_base.h"
#include "fast_const_argument.h"

#include <memory>
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
    using key_size_type = typename bitwise_key::size_type;
    using parent_type = art_node_base<Header>;
    using key_type = typename bitwise_key::key_type;
    using fast_key_type = fast_const_argument_t<key_type>;

    explicit constexpr basic_leaf(fast_key_type key) noexcept
        : parent_type(node_type::LEAF, bitwise_key(key))
    {
    }

    // There is always a single element in this leaf
    [[nodiscard]] static constexpr std::size_t size() noexcept { return 1; }

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

    [[nodiscard]] T& value() noexcept { return *addr(); }
    [[nodiscard]] const T& value() const noexcept { return *addr(); }

    [[noreturn]] static void push_back(T&&) { throw std::runtime_error("basic_leaf: push_back"); }

private:
    [[nodiscard]] T* addr() noexcept { return reinterpret_cast<T*>(&data); }
    [[nodiscard]] const T* addr() const noexcept { return reinterpret_cast<const T*>(&data); }

private:
    std::aligned_storage_t<sizeof(T), alignof(T)> data;
};

// Specialization for integral constants that do not take any space
template <typename Header, typename T, T V, typename Alloc>
struct basic_leaf<Header, std::integral_constant<T, V>, Alloc> final
    : public art_node_base<Header> {
    using value_type = std::integral_constant<T, V>;
    using allocator_type = Alloc;
    using bitwise_key = typename Header::bitwise_key;
    using key_size_type = typename bitwise_key::size_type;
    using parent_type = art_node_base<Header>;
    using key_type = typename bitwise_key::key_type;
    using fast_key_type = fast_const_argument_t<key_type>;

    explicit constexpr basic_leaf(fast_key_type key) noexcept
        : parent_type(node_type::LEAF, bitwise_key(key))
    {
    }

    // There is always a single element in this leaf
    [[nodiscard]] static constexpr std::size_t size() noexcept { return 1; }

    // Does nothing, since the value is a constant expression
    template <typename... Args> static void emplace_value(allocator_type&, Args&&...) noexcept {}
    static void destroy_value(allocator_type&) noexcept {}

    // Simply return a value
    [[nodiscard]] static constexpr value_type value() noexcept { return value_type(); }

    [[noreturn]] static void push_back(value_type)
    {
        throw std::runtime_error("basic_leaf: push_back");
    }
};

} // namespace detail
} // namespace art

#endif // ART_DETAIL_BASIC_LEAF_HEADER_INCLUDED
