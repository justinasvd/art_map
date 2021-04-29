# Copyright 2021 Justinas V. Daugmaudis

cmake_minimum_required(VERSION 3.12)

project(art_bench)

find_package(benchmark REQUIRED)

set(SOURCE_FILES
  art_bench.cpp
)

# Some more general compilation options
add_compile_options(-Wall -Wextra)

add_executable(${PROJECT_NAME}
  ${SOURCE_FILES}
)

target_link_libraries(${PROJECT_NAME}
  benchmark::benchmark
)

# Target include dirs are private to the target
target_include_directories(${PROJECT_NAME} PRIVATE
  ../include
  ..
)
