#ifndef ART_MAP_HEADER_INCLUDED
#define ART_MAP_HEADER_INCLUDED

#include "detail/art_container.h"
#include "detail/container_traits.h"

namespace art
{
namespace detail
{

// Map traits
template <typename Key, typename Data, typename Compare, typename Alloc, typename MultiMap>
struct map_traits
    : public container_traits<Key, std::pair<const Key, Data>, Compare, Alloc, MultiMap> {
    using mapped_type = Data;
    using mutable_value_type = std::pair<Key, Data>;

    using fast_key_type = fast_const_argument_t<Key>;

    // We don't really care what kind of Pair we get here. The biggest requirement is not to
    // make any temporaries (!). So long that the first member of the pair is convertible to
    // fast_key_type, we're all good.
    template <typename Pair> [[nodiscard]] static fast_key_type key(Pair&& x) noexcept
    {
        return x.first;
    }
    template <typename Pair> [[nodiscard]] static auto&& value(Pair&& p) noexcept
    {
        return std::get<1>(std::move(p));
    }
};

} // namespace detail

// Parametrization and special extensions for unique element map
template <typename Key, typename Value, typename Compare = std::less<Key>,
          typename Alloc = std::allocator<Value>>
class map : public detail::db<detail::map_traits<Key, Value, Compare, Alloc, std::false_type>>
{
    using traits_t = detail::map_traits<Key, Value, Compare, Alloc, std::false_type>;
    using base_t = detail::db<traits_t>;

    using fast_key_type = typename traits_t::fast_key_type;

public:
    using key_type = typename base_t::key_type;
    using mapped_type = typename base_t::mapped_type;
    using iterator = typename base_t::iterator;
    using const_iterator = typename base_t::const_iterator;

    // Forward c-tors
    using base_t::db;

    template <typename... Args> std::pair<iterator, bool> emplace(fast_key_type key, Args&&... args)
    {
        return this->emplace_key_args(key, std::forward<Args>(args)...);
    }

    template <typename... Args>
    iterator emplace_hint(const_iterator hint, fast_key_type key, Args&&... args)
    {
        return this->emplace_hint_key_args(hint, key, std::forward<Args>(args)...);
    }

    template <typename... Args>
    std::pair<iterator, bool> try_emplace(fast_key_type key, Args&&... args)
    {
        return this->emplace_key_args(key, std::forward<Args>(args)...);
    }

    template <typename... Args>
    iterator try_emplace(const_iterator hint, fast_key_type key, Args&&... args)
    {
        return this->emplace_hint_key_args(hint, key, std::forward<Args>(args)...);
    }

    // Access specified element with bounds checking.
    mapped_type& at(fast_key_type key) { return internal_at(key)->second; }
    const mapped_type& at(fast_key_type key) const { return internal_at(key)->second; }

    // Insertion routines.
    mapped_type& operator[](fast_key_type key) { return this->try_emplace(key).first->second; }

private:
    iterator internal_at(fast_key_type key) const
    {
        auto it = this->find(key);
        if (it == this->end()) {
            throw std::out_of_range("map::at: key not found");
        }
        return iterator(it);
    }
};

// Parametrization for multimap
template <typename Key, typename Value, typename Compare = std::less<Key>,
          typename Alloc = std::allocator<Value>>
using multimap = detail::db<detail::map_traits<Key, Value, Compare, Alloc, std::true_type>>;

} // namespace art

#endif // ART_MAP_HEADER_INCLUDED
