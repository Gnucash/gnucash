
gnc_add_swig_guile_command (swig-foo-c
    SWIG_FOO_C swig-foo.c
    ${CMAKE_CURRENT_SOURCE_DIR}/foo.i foo.h
)

add_library(foo EXCLUDE_FROM_ALL foo.c foo.h)
add_library(gncmod-foo EXCLUDE_FROM_ALL gnc-mod-foo.c)
set_source_files_properties (gnc-mod-foo.c PROPERTY OBJECT_DEPENDS ${SWIG_FOO_C})

target_link_libraries(gncmod-foo foo gnc-module)
target_include_directories(gncmod-foo PRIVATE
  ${CMAKE_BINARY_DIR}/common
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${CMAKE_CURRENT_BINARY_DIR}
  ${CMAKE_SOURCE_DIR}/common
  ${CMAKE_SOURCE_DIR}/libgnucash/gnc-module
  ${GLIB2_INCLUDE_DIRS}
  ${GUILE_INCLUDE_DIRS}
)

gnc_add_scheme_targets(scm-mod-foo
  gnucash/foo.scm
  "gnucash"
  gncmod-foo
  TRUE
  )

set_target_properties(foo gncmod-foo PROPERTIES
LIBRARY_OUTPUT_DIRECTORY ${LIBDIR_BUILD}/gnucash/test
ARCHIVE_OUTPUT_DIRECTORY ${LIBDIR_BUILD}/gnucash/test
RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

set_dist_list(mod_foo_DIST CMakeLists.txt foo.i gnucash/foo.scm foo.c foo.h gnc-mod-foo.c)
