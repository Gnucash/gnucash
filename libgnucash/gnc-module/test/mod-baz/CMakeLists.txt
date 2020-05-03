
gnc_add_swig_guile_command (swig-baz-c
    SWIG_BAZ_C swig-baz.c
    ${CMAKE_CURRENT_SOURCE_DIR}/baz.i baz.h
)

add_library(baz EXCLUDE_FROM_ALL baz.c baz.h)
target_include_directories(baz PRIVATE
  ${CMAKE_SOURCE_DIR}/libgnucash/gnc-module/test/mod-foo
)
target_link_libraries(baz foo)


add_library(gncmod-baz EXCLUDE_FROM_ALL gnc-mod-baz.c)
set_source_files_properties (gnc-mod-baz.c PROPERTY OBJECT_DEPENDS ${SWIG_BAZ_C})

target_link_libraries(gncmod-baz baz gnc-module)
target_include_directories(gncmod-baz PRIVATE
  ${CMAKE_BINARY_DIR}/common
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${CMAKE_CURRENT_BINARY_DIR}
  ${CMAKE_SOURCE_DIR}/common
  ${CMAKE_SOURCE_DIR}/libgnucash/gnc-module
  ${GLIB2_INCLUDE_DIRS}
  ${GUILE_INCLUDE_DIRS}
)

gnc_add_scheme_targets(scm-mod-baz
  gnucash/baz.scm
  "gnucash"
  gncmod-baz
  TRUE
  )

set_target_properties(baz gncmod-baz PROPERTIES
LIBRARY_OUTPUT_DIRECTORY ${LIBDIR_BUILD}/gnucash/test
ARCHIVE_OUTPUT_DIRECTORY ${LIBDIR_BUILD}/gnucash/test
RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

set_dist_list(mod_baz_DIST CMakeLists.txt baz.c gnucash/baz.scm baz.h baz.i gnc-mod-baz.c)
