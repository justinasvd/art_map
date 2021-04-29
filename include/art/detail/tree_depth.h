#ifndef ART_DETAIL_TREE_DEPTH_HEADER_INCLUDED
#define ART_DETAIL_TREE_DEPTH_HEADER_INCLUDED

#include <cassert>
#include <cstdint>

namespace art
{
namespace detail
{

class tree_depth final
{
public:
    using size_type = std::size_t;

    explicit constexpr tree_depth(size_type max_depth, size_type depth = 0) noexcept
        : depth{depth}
        , max_depth{max_depth}
    {
        assert(depth <= max_depth);
    }

    [[nodiscard]] constexpr operator size_type() const noexcept { return depth; }

    constexpr tree_depth& operator++() noexcept
    {
        assert(depth != max_depth);
        ++depth;
        return *this;
    }

    constexpr tree_depth& operator+=(size_type delta) noexcept
    {
        assert(depth + delta <= max_depth);
        depth += delta;
        return *this;
    }

private:
    size_type depth, max_depth;
};

} // namespace detail
} // namespace art

#endif // ART_DETAIL_TREE_DEPTH_HEADER_INCLUDED
