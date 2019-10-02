add_subdirectory(test)

# Command to generate the swig-core-utils-guile.c wrapper file
gnc_add_swig_guile_command (swig-core-utils-guile-c
    SWIG_CORE_UTILS_GUILE_C swig-core-utils-guile.c
    ${CMAKE_SOURCE_DIR}/bindings/core-utils.i ${CMAKE_SOURCE_DIR}/libgnucash/core-utils
)

set(guile_HEADERS
    gnc-guile-utils.h)

set(guile_SOURCES
    gnc-guile-bindings.c
    gnc-guile-utils.c)

add_library(gnucash-guile SHARED
    ${guile_SOURCES}
    ${guile_HEADERS}
    ${SWIG_CORE_UTILS_GUILE_C})

add_dependencies(gnucash-guile
    swig-runtime-h)

target_include_directories(gnucash-guile
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}
        ${GUILE_INCLUDE_DIRS}
        ${GLIB2_INCLUDE_DIRS}
    PRIVATE
        ${CMAKE_SOURCE_DIR}/common
        ${CMAKE_BINARY_DIR}/common)

target_link_libraries(gnucash-guile
    PUBLIC
        ${GUILE_LDFLAGS}
    PRIVATE
        gnc-core-utils
        ${GLIB2_LDFLAGS})

install(TARGETS gnucash-guile
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)

# Scheme

set(GUILE_DEPENDS      scm-core-utils scm-gnc-module gncmod-engine)

gnc_add_scheme_targets(scm-scm
  "utilities.scm"
  gnucash
  "${GUILE_DEPENDS}"
  FALSE
)

# depends on main.scm
gnc_add_scheme_targets(price-quotes
  price-quotes.scm
  gnucash
  "scm-scm;scm-app-utils"
  FALSE)

set (core_utils_SCHEME core-utils.scm)

set(GUILE_OUTPUT_DIR   gnucash)
set(GUILE_DEPENDS      gnc-core-utils gnucash-guile)

gnc_add_scheme_targets(scm-core-utils
    "${core_utils_SCHEME}"
    ${GUILE_OUTPUT_DIR}
    "${GUILE_DEPENDS}"
    FALSE
)

set_local_dist(guile_DIST_local
    CMakeLists.txt
    core-utils.scm
    utilities.scm
    price-quotes.scm
    ${guile_SOURCES}
    ${guile_HEADERS})
set(guile_DIST ${guile_DIST_local} ${test_guile_DIST} PARENT_SCOPE)


