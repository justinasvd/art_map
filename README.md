## Introduction

This library is a C++14 template library and, as such, there is no
library to build and install. It is heavily based on Laurynas Biveinis'
excellent implementation of an adaptive radix tree [[1]](#1). See his
implementation of in-memory database at https://github.com/laurynas-biveinis/unodb.

There are notable differences from the original implementation. They mostly
stem from the fact that both `art::set` and `art::map` are aimed to be
drop-in replacements for `std::set` and `std::map` respectively. This means
that `art::set` and `art::map` must meet the requirements of `Container`,
`AllocatorAwareContainer`, `AssociativeContainer` and `ReversibleContainer`,
which `unodb` does not try to do.

## Caveats

There are limits to what kinds of keys can be used in ART. Keys must be
transformable to `BitwiseComparable`, maintaining the order imposed by
`std::less<Key>` or `std::greater<Key>`. Such transformations for integral
key types, pointers and `std::string` (in `std::less<std::string>` case)
are already provided. In theory, it is possible to provide bitwise orderings
for floating point types and, perhaps, composite types as well. We have not
ventured there. Patches welcome.

Also, insertions and deletions on an Adaptive Radix Tree can cause nodes to resize.
The result is that insertions and deletions can invalidate iterators
pointing to values other than the one being inserted/deleted. This is
notably different from libstdc++ set/map which take care to not invalidate
iterators on insert/erase except, of course, for iterators pointing to the
value being erased.

## Requirements

The code uses SSE4.1 intrinsics (Nehalem and higher). This is in contrast to
the original ART paper needing SSE2 only.

The following minimum versions are required to build the library:

* GCC 6.1
* Clang 3.4

See [Platform-Specific Build Instructions](#platform-specific-build-instructions).

## Building tests and benchmarks

This describes the build process using cmake. As pre-requisites, you'll
need git and cmake installed.

```bash
# Check out the library.
$ git clone https://github.com/justinasvd/art_map.git
# Go to the library root directory
$ cd art_map
# Make a build directory to place the build output.
$ cmake -E make_directory "build"
# Generate build system files with cmake.
$ cmake -E chdir "build" cmake -DCMAKE_BUILD_TYPE=Release ../
# or, starting with CMake 3.13, use a simpler form:
# cmake -DCMAKE_BUILD_TYPE=Release -S . -B "build"
# Build the library.
$ cmake --build "build" --config Release
```

## References
<a id="1">[1]</a>
Leis, Viktor, Alfons Kemper, and Thomas Neumann. "The adaptive radix tree: ARTful indexing for main-memory databases." 2013 IEEE 29th International Conference on Data Engineering (ICDE). IEEE, 2013.
