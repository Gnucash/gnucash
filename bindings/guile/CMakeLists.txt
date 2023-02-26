add_subdirectory(test)

# Generate the swig-core-utils-guile.c wrapper file
gnc_swig_extract_header_files (gnc-core-utils CORE_UTILS_HEADERS)
gnc_add_swig_guile_command (swig-core-utils-guile-c
    SWIG_CORE_UTILS_GUILE_C swig-core-utils-guile.c
    ${CMAKE_SOURCE_DIR}/bindings/core-utils.i
    ${CMAKE_SOURCE_DIR}/libgnucash/core-utils
    ${CORE_UTILS_HEADERS}
)

set(SWIG_ARGS "-c++" "-procdoc" "sw-gnc-option-doc" "-procdocformat" "plain")
# Generate the swig-engine.cpp wrapper file
gnc_swig_extract_header_files (gnc-engine ENGINE_HEADERS)
gnc_add_swig_guile_command (swig-engine-cpp
    SWIG_ENGINE_CPP swig-engine.cpp
    ${CMAKE_SOURCE_DIR}/bindings/engine.i
    "${CMAKE_CURRENT_SOURCE_DIR};${CMAKE_SOURCE_DIR}/libgnucash/engine"
    ${ENGINE_HEADERS}
    ${CMAKE_SOURCE_DIR}/bindings/business-core.i
    ${CMAKE_SOURCE_DIR}/bindings/engine-common.i
    ${CMAKE_CURRENT_SOURCE_DIR}/gnc-optiondb.i #additional dependencies
)

# Generate the swig-app-utils-guile.cpp wrapper file
gnc_add_swig_guile_command (swig-apputils-guile-cpp #target
        SWIG_APP_UTILS_GUILE_CPP swig-app-utils-guile.cpp #outvar, output
        ${CMAKE_CURRENT_SOURCE_DIR}/../app-utils.i #input
        "${CMAKE_CURRENT_SOURCE_DIR};${CMAKE_SOURCE_DIR}/libgnucash/app-utils" #includes
        )
unset(SWIG_ARGS)

#Generate the swig-expressions-guile.c wrapper file
gnc_add_swig_guile_command(swig-expressions-guile-c #target
        SWIG_EXPRESSIONS_GUILE_C swig-expressions-guile.c #outvar, output
        ${CMAKE_CURRENT_SOURCE_DIR}/expressions.i #input
        "${CMAKE_CURRENT_SOURCE_DIR};${CMAKE_SOURCE_DIR}/libgnucash/app-utils;${CMAKE_SOURCE_DIR}/libgnucash/expressions" #includes
        )

set(guile_HEADERS
    glib-guile.h
    gnc-engine-guile.h
    gnc-guile-utils.h
    gnc-helpers.h
    gnc-kvp-guile.h)

set(guile_SOURCES
    glib-guile.c
    gnc-engine-guile.cpp
    gnc-guile-bindings.c
    gnc-guile-utils.c
    gnc-helpers.c
    gnc-kvp-guile.cpp)

add_library(gnucash-guile SHARED
    ${guile_SOURCES}
    ${guile_HEADERS}
    ${SWIG_CORE_UTILS_GUILE_C}
    ${SWIG_ENGINE_CPP}
    ${SWIG_APP_UTILS_GUILE_CPP})

add_dependencies(gnucash-guile
    swig-runtime-h
    swig-core-utils-guile-c
    swig-engine-cpp
    swig-apputils-guile-cpp)

target_include_directories(gnucash-guile
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}
        ${GUILE_INCLUDE_DIRS}
    PRIVATE
        ${CMAKE_SOURCE_DIR}/common
        ${CMAKE_BINARY_DIR}/common)

target_link_libraries(gnucash-guile
    PUBLIC
        ${GUILE_LDFLAGS}
    PRIVATE
        gnc-core-utils
        gnc-engine
        gnc-app-utils
        PkgConfig::GLIB2)

install(TARGETS gnucash-guile
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)

add_library(gnc-expressions-guile SHARED
        ${SWIG_EXPRESSIONS_GUILE_C})

target_include_directories(gnc-expressions-guile
        PUBLIC
        ${CMAKE_SOURCE_DIR}/libgnucash/expressions
        ${GUILE_INCLUDE_DIRS})

target_link_libraries(gnc-expressions-guile
        gnc-expressions
        gnc-engine
        gnc-app-utils
        ${GUILE_LDFLAGS}
        PkgConfig::GLIB2)

install(TARGETS gnc-expressions-guile
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
        ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
        )
add_dependencies(gnc-expressions-guile swig-expressions-guile-c)

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
    DEPENDS "${GUILE_DEPENDS}"
    MAKE_LINKS)

set (engine_SCHEME_0
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
    DEPENDS "scm-core-utils;scm-engine-0;${GUILE_DEPENDS}"
    MAKE_LINKS)

gnc_add_scheme_targets(scm-engine-2
    SOURCES "${engine_SCHEME_2}"
    OUTPUT_DIR gnucash
    DEPENDS "scm-engine-1;${GUILE_DEPENDS}"
    MAKE_LINKS)


gnc_add_scheme_targets(scm-options
  SOURCES options.scm
  OUTPUT_DIR gnucash
  DEPENDS "scm-engine-2;scm-core-utils;${GUILE_DEPENDS}")

add_custom_target(scm-engine ALL DEPENDS scm-options scm-engine-2 scm-engine-1 scm-engine-0)

set (app_utils_SCHEME_1
        c-interface.scm
        date-utilities.scm
        )

set (app_utils_SCHEME_2
        app-utils.scm
        )


set(GUILE_DEPENDS
        gnc-app-utils
        scm-core-utils
        scm-engine)

gnc_add_scheme_targets(scm-app-utils-1
        SOURCES "${app_utils_SCHEME_1}"
        OUTPUT_DIR "gnucash/app-utils"
        DEPENDS "${GUILE_DEPENDS}"
        MAKE_LINKS)

gnc_add_scheme_targets(scm-app-utils-2
        SOURCES "${app_utils_SCHEME_2}"
        OUTPUT_DIR "gnucash"
        DEPENDS "scm-app-utils-1"
        MAKE_LINKS)

add_custom_target(scm-app-utils ALL DEPENDS scm-app-utils-2 scm-app-utils-1)

set(expressions_SCHEME
        fin.scm)

set(GUILE_INTERNAL_DEPENDS
        scm-app-utils-1)

gnc_add_scheme_targets(scm-expressions
        SOURCES "${expressions_SCHEME}"
        OUTPUT_DIR "gnucash/app-utils"
        DEPENDS "${GUILE_INTERNAL_DEPENDS}"
        MAKE_LINKS)

set_local_dist(guile_DIST_local
    CMakeLists.txt
    core-utils.scm
    options.scm
    ${guile_SOURCES}
    ${guile_HEADERS}
    ${engine_SCHEME_0}
    ${engine_SCHEME_1}
    ${engine_SCHEME_2}
    ${app_utils_SCHEME_1}
    ${app_utils_SCHEME_1a}
    ${app_utils_SCHEME_1b}
    ${app_utils_SCHEME_1c}
    ${app_utils_SCHEME_2}
    ${app_utils_SCHEME_3}
    ${expressions_SCHEME}
    gnc-optiondb.i expressions.i)
set(guile_DIST ${guile_DIST_local} ${test_guile_DIST} PARENT_SCOPE)


