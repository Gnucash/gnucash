add_subdirectory(test)

# Command to generate the swig-core-utils-guile.c wrapper file
gnc_swig_extract_header_files (gnc-core-utils CORE_UTILS_HEADERS)
gnc_add_swig_guile_command (swig-core-utils-guile-c
    SWIG_CORE_UTILS_GUILE_C swig-core-utils-guile.c
    ${CMAKE_SOURCE_DIR}/bindings/core-utils.i
    ${CMAKE_SOURCE_DIR}/libgnucash/core-utils
    ${CORE_UTILS_HEADERS}
)

# Command to generate the swig-engine.c wrapper file
gnc_swig_extract_header_files (gnc-engine ENGINE_HEADERS)
gnc_add_swig_guile_command (swig-engine-c
    SWIG_ENGINE_C swig-engine.c
    ${CMAKE_SOURCE_DIR}/bindings/engine.i
    "${CMAKE_CURRENT_SOURCE_DIR};${CMAKE_SOURCE_DIR}/libgnucash/engine"
    ${ENGINE_HEADERS}
    ${CMAKE_SOURCE_DIR}/bindings/business-core.i
    ${CMAKE_SOURCE_DIR}/bindings/engine-common.i
)

# Command to generate the swig-gnc-module.c wrapper file
gnc_swig_extract_header_files (gnc-module GNC_MODULE_HEADERS)
gnc_add_swig_guile_command (swig-gnc-module-c
    SWIG_GNC_MODULE_C swig-gnc-module.c
    ${CMAKE_SOURCE_DIR}/bindings/gnc-module.i
    "${CMAKE_CURRENT_SOURCE_DIR};${CMAKE_SOURCE_DIR}/libgnucash/gnc-module"
    ${GNC_MODULE_HEADERS}
)

set(guile_HEADERS
    glib-guile.h
    gnc-engine-guile.h
    gnc-guile-utils.h
    gnc-kvp-guile.h)

set(guile_SOURCES
    glib-guile.c
    gnc-engine-guile.c
    gnc-guile-bindings.c
    gnc-guile-utils.c
    gnc-kvp-guile.cpp)

add_library(gnucash-guile SHARED
    ${guile_SOURCES}
    ${guile_HEADERS}
    ${SWIG_CORE_UTILS_GUILE_C}
    ${SWIG_ENGINE_C}
    ${SWIG_GNC_MODULE_C})

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
        gnc-engine
        gnc-module
        ${GLIB2_LDFLAGS})

install(TARGETS gnucash-guile
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)

# Scheme

set (core_utils_SCHEME core-utils.scm)

set(GUILE_OUTPUT_DIR   gnucash)
set(GUILE_DEPENDS      gnc-core-utils gnucash-guile)

gnc_add_scheme_targets(scm-core-utils
    "${core_utils_SCHEME}"
    ${GUILE_OUTPUT_DIR}
    "${GUILE_DEPENDS}"
    FALSE
)

set (engine_SCHEME_0
    commodity-table.scm
    gnc-numeric.scm
    business-core.scm
)

set (engine_SCHEME_1
    engine.scm
)

set (engine_SCHEME_2
    utilities.scm
)

set(BACKEND_DEPENDS gncmod-backend-xml)
if (WITH_SQL)
    list(APPEND BACKEND_DEPENDS gncmod-backend-dbi)
endif(WITH_SQL)

set(GUILE_DEPENDS
  ${BACKEND_DEPENDS}
  gnc-engine
  gnucash-guile)


gnc_add_scheme_targets(scm-engine-0
    "${engine_SCHEME_0}"
    "gnucash/engine"
    "${GUILE_DEPENDS}"
    TRUE
)

gnc_add_scheme_targets(scm-engine-1
    "${engine_SCHEME_1}"
    gnucash
    "scm-engine-0;${GUILE_DEPENDS}"
    FALSE
)

gnc_add_scheme_targets(scm-engine-2
    "${engine_SCHEME_2}"
    gnucash
    "scm-engine-1;${GUILE_DEPENDS}"
    FALSE
)

add_custom_target(scm-engine ALL DEPENDS scm-engine-2 scm-engine-1 scm-engine-0)

set(scm_gnc_module_DEPENDS
    gnc-module
    gnucash-guile)

gnc_add_scheme_targets(scm-gnc-module
    gnc-module.scm
    gnucash
    "${scm_gnc_module_DEPENDS}"
    FALSE
)

gnc_add_scheme_targets(price-quotes
    price-quotes.scm
    gnucash
    "scm-engine;scm-app-utils;scm-gnome-utils"
    FALSE)

set_local_dist(guile_DIST_local
    CMakeLists.txt
    core-utils.scm
    gnc-module.scm
    price-quotes.scm
    ${guile_SOURCES}
    ${guile_HEADERS}
    ${engine_SCHEME_0}
    ${engine_SCHEME_1}
    ${engine_SCHEME_2})
set(guile_DIST ${guile_DIST_local} ${test_guile_DIST} PARENT_SCOPE)


