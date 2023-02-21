
add_library(ordinary EXCLUDE_FROM_ALL ordinary.c ordinary.h)
add_library(gncmod-ordinary EXCLUDE_FROM_ALL gnc-mod-ordinary.c)

target_link_libraries(gncmod-ordinary ordinary gnc-module)
target_include_directories(gncmod-ordinary PRIVATE
  ${CMAKE_BINARY_DIR}/common
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${CMAKE_CURRENT_BINARY_DIR}
  ${CMAKE_SOURCE_DIR}/common
  ${CMAKE_SOURCE_DIR}/libgnucash/gnc-module
  ${GUILE_INCLUDE_DIRS}
)

set_target_properties(ordinary gncmod-ordinary PROPERTIES
LIBRARY_OUTPUT_DIRECTORY ${LIBDIR_BUILD}/gnucash/test
ARCHIVE_OUTPUT_DIRECTORY ${LIBDIR_BUILD}/gnucash/test
RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin/test)

set_dist_list(mod_ordinary_DIST CMakeLists.txt ordinary.c ordinary.h gnc-mod-ordinary.c)
