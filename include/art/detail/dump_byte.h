#ifndef ART_DETAIL_DUMP_BYTE_HEADER_INCLUDED
#define ART_DETAIL_DUMP_BYTE_HEADER_INCLUDED

#include <ostream>

namespace art
{
namespace detail
{

inline void dump_byte(std::ostream& os, std::uint8_t byte)
{
    char hex[4];
    std::snprintf(hex, 4, " %2x", static_cast<unsigned>(byte));
    os.write(hex, 3);
}

} // namespace detail
} // namespace art

#endif // ART_DETAIL_DUMP_BYTE_HEADER_INCLUDED
