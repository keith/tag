project(tag LANGUAGES CXX)
cmake_minimum_required(VERSION 3.21)

set(CMAKE_EXPORT_COMPILE_COMMANDS 1)
set(CMAKE_CXX_STANDARD 20)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE
      "RelWithDebInfo"
      CACHE STRING "Default build type: RelWithDebInfo" FORCE)
endif()

add_executable(tag tag.cpp)
target_compile_options(
  tag
  PRIVATE -g
          -Weverything
          -Wno-c++98-compat
          -Wno-exit-time-destructors
          -Wno-global-constructors
          -Wno-padded
          -fno-exceptions)
install(TARGETS tag)
