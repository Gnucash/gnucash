
gnc_add_swig_guile_command (swig-foo-c
    SWIG_FOO_C swig-foo.c
    ${CMAKE_CURRENT_SOURCE_DIR}/foo.i foo.h
)

ADD_LIBRARY(foo EXCLUDE_FROM_ALL foo.c foo.h)
ADD_LIBRARY(gncmod-foo EXCLUDE_FROM_ALL gnc-mod-foo.c)
SET_SOURCE_FILES_PROPERTIES (gnc-mod-foo.c PROPERTY OBJECT_DEPENDS ${SWIG_FOO_C})

TARGET_LINK_LIBRARIES(gncmod-foo foo gnc-module)
TARGET_INCLUDE_DIRECTORIES(gncmod-foo PRIVATE
  ${CMAKE_BINARY_DIR}/common
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${CMAKE_CURRENT_BINARY_DIR}
  ${CMAKE_SOURCE_DIR}/common
  ${CMAKE_SOURCE_DIR}/libgnucash/gnc-module
  ${GLIB2_INCLUDE_DIRS}
  ${GUILE_INCLUDE_DIRS}
)

GNC_ADD_SCHEME_TARGETS(scm-mod-foo
  gnucash/foo.scm
  "gnucash"
  gncmod-foo
  TRUE
  )

SET_TARGET_PROPERTIES(foo gncmod-foo PROPERTIES
LIBRARY_OUTPUT_DIRECTORY ${LIBDIR_BUILD}/gnucash/test
ARCHIVE_OUTPUT_DIRECTORY ${LIBDIR_BUILD}/gnucash/test
RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

SET_DIST_LIST(mod_foo_DIST CMakeLists.txt foo.i gnucash/foo.scm foo.c foo.h gnc-mod-foo.c)
