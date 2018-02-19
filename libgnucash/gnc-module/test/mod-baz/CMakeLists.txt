
gnc_add_swig_guile_command (swig-baz-c
    SWIG_BAZ_C swig-baz.c
    ${CMAKE_CURRENT_SOURCE_DIR}/baz.i baz.h
)

ADD_LIBRARY(baz EXCLUDE_FROM_ALL baz.c baz.h)
TARGET_INCLUDE_DIRECTORIES(baz PRIVATE
  ${CMAKE_SOURCE_DIR}/libgnucash/gnc-module/test/mod-foo
)
TARGET_LINK_LIBRARIES(baz foo)


ADD_LIBRARY(gncmod-baz EXCLUDE_FROM_ALL gnc-mod-baz.c)
SET_SOURCE_FILES_PROPERTIES (gnc-mod-baz.c PROPERTY OBJECT_DEPENDS ${SWIG_BAZ_C})

TARGET_LINK_LIBRARIES(gncmod-baz baz gnc-module)
TARGET_INCLUDE_DIRECTORIES(gncmod-baz PRIVATE
  ${CMAKE_BINARY_DIR}/common
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${CMAKE_CURRENT_BINARY_DIR}
  ${CMAKE_SOURCE_DIR}/common
  ${CMAKE_SOURCE_DIR}/libgnucash/gnc-module
  ${GLIB2_INCLUDE_DIRS}
  ${GUILE_INCLUDE_DIRS}
)

GNC_ADD_SCHEME_TARGETS(scm-mod-baz
  gnucash/baz.scm
  "gnucash"
  gncmod-baz
  TRUE
  )

SET_TARGET_PROPERTIES(baz gncmod-baz PROPERTIES
LIBRARY_OUTPUT_DIRECTORY ${LIBDIR_BUILD}/gnucash/test
ARCHIVE_OUTPUT_DIRECTORY ${LIBDIR_BUILD}/gnucash/test
RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

SET_DIST_LIST(mod_baz_DIST CMakeLists.txt baz.c gnucash/baz.scm baz.h baz.i gnc-mod-baz.c)
