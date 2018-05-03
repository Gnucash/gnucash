
gnc_add_swig_guile_command (swig-bar-c
    SWIG_BAR_C swig-bar.c
    ${CMAKE_CURRENT_SOURCE_DIR}/bar.i bar.h
)

add_library(bar EXCLUDE_FROM_ALL bar.c bar.h)
add_library(gncmod-bar EXCLUDE_FROM_ALL gnc-mod-bar.c)
set_source_files_properties (gnc-mod-bar.c PROPERTY OBJECT_DEPENDS ${SWIG_BAR_C})

target_link_libraries(gncmod-bar bar gnc-module)
target_include_directories(gncmod-bar PRIVATE
  ${CMAKE_BINARY_DIR}/common
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${CMAKE_CURRENT_BINARY_DIR}
  ${CMAKE_SOURCE_DIR}/common
  ${CMAKE_SOURCE_DIR}/libgnucash/gnc-module
  ${GLIB2_INCLUDE_DIRS}
  ${GUILE_INCLUDE_DIRS}
)

set_target_properties(bar gncmod-bar PROPERTIES
  LIBRARY_OUTPUT_DIRECTORY ${LIBDIR_BUILD}/gnucash/test
  ARCHIVE_OUTPUT_DIRECTORY ${LIBDIR_BUILD}/gnucash/test
  RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
  )

set_dist_list(mod_bar_DIST CMakeLists.txt bar.c gnucash/bar.scm bar.h bar.i gnc-mod-bar.c)
