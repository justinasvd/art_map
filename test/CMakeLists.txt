# Copyright 2021 Justinas V. Daugmaudis

cmake_minimum_required(VERSION 3.12)

project(art_test)

set(SOURCE_FILES
  art_test.cpp
)

# Some more general compilation options
add_compile_options(-Wall -Wextra)

add_executable(${PROJECT_NAME}
  ${SOURCE_FILES}
)

# Target include dirs are private to the target
target_include_directories(${PROJECT_NAME} PRIVATE
  ../include
)
