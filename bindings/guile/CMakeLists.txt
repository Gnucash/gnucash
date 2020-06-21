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
    ${SWIG_ENGINE_C})

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
        ${GLIB2_LDFLAGS})

install(TARGETS gnucash-guile
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)

# Scheme

set (core_utils_SCHEME core-utils.scm)

set(GUILE_OUTPUT_DIR   gnucash)
set(GUILE_DEPENDS
    gnc-core-utils
    gnucash-guile
    gncmod-backend-xml)

if(WITH_SQL)
    list(APPEND GUILE_DEPENDS gncmod-backend-dbi)
endif()

gnc_add_scheme_targets(scm-core-utils
    SOURCES "${core_utils_SCHEME}"
    OUTPUT_DIR ${GUILE_OUTPUT_DIR}
    DEPENDS "${GUILE_DEPENDS}")

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
endif()

set(GUILE_DEPENDS
  ${BACKEND_DEPENDS}
  gnc-engine
  gnucash-guile)


gnc_add_scheme_targets(scm-engine-0
    SOURCES "${engine_SCHEME_0}"
    OUTPUT_DIR "gnucash/engine"
    DEPENDS "${GUILE_DEPENDS}"
    MAKE_LINKS)

gnc_add_scheme_targets(scm-engine-1
    SOURCES "${engine_SCHEME_1}"
    OUTPUT_DIR gnucash
    DEPENDS "scm-core-utils;scm-engine-0;${GUILE_DEPENDS}")

gnc_add_scheme_targets(scm-engine-2
    SOURCES "${engine_SCHEME_2}"
    OUTPUT_DIR gnucash
    DEPENDS "scm-engine-1;${GUILE_DEPENDS}")

add_custom_target(scm-engine ALL DEPENDS scm-engine-2 scm-engine-1 scm-engine-0)

set(scm_gnc_module_DEPENDS
    gnucash-guile)

gnc_add_scheme_targets(scm-gnc-module
    SOURCES gnc-module.scm
    OUTPUT_DIR gnucash
    DEPENDS "${scm_gnc_module_DEPENDS}")

set_local_dist(guile_DIST_local
    CMakeLists.txt
    core-utils.scm
    gnc-module.scm
    ${guile_SOURCES}
    ${guile_HEADERS}
    ${engine_SCHEME_0}
    ${engine_SCHEME_1}
    ${engine_SCHEME_2})
set(guile_DIST ${guile_DIST_local} ${test_guile_DIST} PARENT_SCOPE)


