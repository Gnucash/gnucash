
add_library(ctre INTERFACE)

target_include_directories(ctre INTERFACE ${CMAKE_CURRENT_SOURCE_DIR})

set(ctre_DIST ${ctre_DIST_LOCAL} PARENT_SCOPE)

set_local_dist(ctre_DIST_LOCAL
  CMakeLists.txt
  ctre.hpp
)

set(ctre_DIST ${ctre_DIST_LOCAL} PARENT_SCOPE)
