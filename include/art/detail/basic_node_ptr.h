#ifndef ART_DETAIL_BASIC_NODE_PTR_HEADER_INCLUDED
#define ART_DETAIL_BASIC_NODE_PTR_HEADER_INCLUDED

#include <cstddef>

namespace art
{
namespace detail
{

// A pointer to some kind of node. It can be accessed either as a leaf, or as one of the internal
// nodes. This depends on all types being of standard layout and Header being at the same location
// in all node. This is checked by static asserts in the implementation file.
template <class Leaf, class INode, class Node4, class Node16, class Node48, class Node256>
union basic_node_ptr {
    using leaf_type = Leaf;
    using inode = INode;
    using inode4_type = Node4;
    using inode16_type = Node16;
    using inode48_type = Node48;
    using inode256_type = Node256;

    leaf_type* leaf;
    inode* internal;
    inode4_type* node_4;
    inode16_type* node_16;
    inode48_type* node_48;
    inode256_type* node_256;

    basic_node_ptr() = default;
    explicit constexpr basic_node_ptr(std::nullptr_t) noexcept
        : leaf{nullptr}
    {
    }
    explicit constexpr basic_node_ptr(leaf_type* leaf_) noexcept
        : leaf{leaf_}
    {
    }
    explicit constexpr basic_node_ptr(inode4_type* node_4_) noexcept
        : node_4{node_4_}
    {
    }
    explicit constexpr basic_node_ptr(inode16_type* node_16_) noexcept
        : node_16{node_16_}
    {
    }
    explicit constexpr basic_node_ptr(inode48_type* node_48_) noexcept
        : node_48{node_48_}
    {
    }
    explicit constexpr basic_node_ptr(inode256_type* node_256_) noexcept
        : node_256{node_256_}
    {
    }

    [[nodiscard]] constexpr node_type type() const noexcept { return internal->type(); }

    [[nodiscard]] constexpr bool operator==(std::nullptr_t) const noexcept
    {
        return leaf == nullptr;
    }
    [[nodiscard]] constexpr bool operator!=(std::nullptr_t) const noexcept
    {
        return leaf != nullptr;
    }
};

} // namespace detail
} // namespace art

#endif // ART_DETAIL_BASIC_NODE_PTR_HEADER_INCLUDED
