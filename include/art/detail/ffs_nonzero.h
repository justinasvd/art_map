#ifndef ART_DETAIL_FFS_NONZERO_HEADER_INCLUDED
#define ART_DETAIL_FFS_NONZERO_HEADER_INCLUDED

#include <cstdint>

namespace art
{
namespace detail
{

// On x86_64 with GCC up to and including version 10, __builtin_ffs compiles to
// BSF/CMOVE pair if TZCNT is not available. CMOVE is only required if arg is
// zero, which we know not to be. Only GCC 11 gets the hint by "if (arg == 0)
// __builtin_unreachable()"
inline unsigned int ffs_nonzero(std::uint64_t arg) noexcept
{
#ifdef __x86_64
    std::int64_t result;
    __asm__("bsfq %1, %0" : "=r"(result) : "rm"(arg) : "cc");
    return static_cast<unsigned>(result + 1);
#else
    return static_cast<unsigned>(__builtin_ffsl(static_cast<std::int64_t>(arg)));
#endif
}

inline unsigned int ffs_nonzero(std::uint32_t arg) noexcept
{
#ifdef __x86_64
    std::uint32_t result;
    __asm__("bsfl %1, %0" : "=r"(result) : "rm"(arg) : "cc");
    return result + 1;
#else
    return __builtin_ffs(static_cast<std::int32_t>(arg));
#endif
}

} // namespace detail
} // namespace art

#endif // ART_DETAIL_FFS_NONZERO_HEADER_INCLUDED
