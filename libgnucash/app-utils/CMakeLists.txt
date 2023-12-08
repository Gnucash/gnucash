# NB: Unit tests which require GSchemas should be made conditional on COMPILE_GSCHEMAS.
add_subdirectory(test)
add_subdirectory(mocks)

include (GncFindLibm)

# Build the library

set (app_utils_HEADERS
  QuickFill.h
  file-utils.h
  gnc-account-merge.h
  gnc-addr-quickfill.h
  gnc-entry-quickfill.h
  gnc-gsettings.h
  gnc-help-utils.h
  gnc-prefs-utils.h
  gnc-quotes.hpp
  gnc-state.h
  gnc-ui-util.h
  gnc-ui-balances.h
)

set (app_utils_SOURCES
  QuickFill.c
  file-utils.c
  gnc-account-merge.c
  gnc-addr-quickfill.c
  gnc-entry-quickfill.c
  gnc-gsettings.cpp
  gnc-prefs-utils.c
  gnc-quotes.cpp
  gnc-state.c
  gnc-ui-util.c
  gnc-ui-balances.c
  )

set_source_files_properties (${app_utils_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

gnc_check_standard_math_library()
if (NOT STANDARD_MATH_LIBRARY_FOUND)
  message(FATAL_ERROR "An implementation of the standard C function pow() is required and is supported neither by the C runtime nor libm.so.")
endif()

set(app_utils_ALL_SOURCES ${app_utils_SOURCES} ${app_utils_HEADERS})
set(app_utils_ALL_LIBRARIES
    gnc-engine
    ${Boost_FILESYSTEM_LIBRARY}
    ${Boost_PROPERTY_TREE_LIBRARY}
    ${Boost_LOCALE_LIBRARY}
    ${GIO_LDFLAGS}
    ${LIBXML2_LDFLAGS}
    ${LIBXSLT_LDFLAGS}
    ${STANDARD_MATH_LIBRARY}
)

set(app_utils_ALL_INCLUDES
    ${CMAKE_CURRENT_SOURCE_DIR}/calculation
    ${GIO_INCLUDE_DIRS}
    ${LIBXML2_INCLUDE_DIRS}
    ${LIBXSLT_INCLUDE_DIRS}
    ${CMAKE_SOURCE_DIR}/libgnucash/backend
    ${CMAKE_SOURCE_DIR}/libgnucash/engine)

if (WIN32)
  list(APPEND app_utils_ALL_SOURCES gnc-help-utils.c)
  list(APPEND app_utils_ALL_LIBRARIES ${HTMLHELP_LIBRARY} "-lwsock32")
  list(APPEND app_utils_ALL_INCLUDES ${HTMLHELP_INCLUDE_PATH})
endif()


add_library (gnc-app-utils ${app_utils_ALL_SOURCES} ${SWIG_APP_UTILS_GUILE_CPP})
target_link_libraries(gnc-app-utils ${app_utils_ALL_LIBRARIES})

target_include_directories (gnc-app-utils
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}
    PRIVATE
        ${app_utils_ALL_INCLUDES}
)
target_compile_definitions (gnc-app-utils PRIVATE -DG_LOG_DOMAIN=\"gnc.app-utils\")

if (APPLE)
  set_target_properties (gnc-app-utils PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}")
endif()

if (COVERAGE)
  add_coverage_target(gnc-app-utils)
endif()

install (TARGETS gnc-app-utils
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)

set(expressions_SOURCES
        calculation/expression_parser.c
        calculation/fin.c
        gfec.c
        gnc-exp-parser.c
        gnc-sx-instance-model.c
        )

set(expressions_noinstall_HEADERS
        calculation/finvar.h
        calculation/finproto.h
        calculation/fin_spl_protos.h
        calculation/fin_static_proto.h
        )
set(expressions_HEADERS
        gfec.h
        gnc-exp-parser.h
        gnc-sx-instance-model.h
        )

set(expressions_all_sources ${expressions_SOURCES} ${expressions_noinstall_HEADERS} ${expressions_HEADERS})

set_source_files_properties(${expressions_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})
add_library(gnc-expressions
        ${expressions_HEADERS}
        ${expressions_SOURCES}
        )

target_include_directories(gnc-expressions
        PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}
        ${CMAKE_CURRENT_SOURCE_DIR}/calculation
        ${CMAKE_SOURCE_DIR}/bindings/guile
        ${CMAKE_SOURCE_DIR}/libgnucash/app-utils
        ${GUILE_INCLUDE_DIRS}
        PRIVATE
        ${CMAKE_SOURCE_DIR}/common
        ${CMAKE_BINARY_DIR}/common)

target_link_libraries(gnc-expressions
        PUBLIC
        gnc-engine
        gnc-app-utils
        gnucash-guile
        ${GUILE_LDFLAGS}
        PkgConfig::GLIB2)

install(TARGETS gnc-expressions
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
        ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
        )


install(FILES ${app_utils_HEADERS} ${expressions_HEADERS}
        DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/gnucash)
set_local_dist(app_utils_DIST_local
    ${app_utils_ALL_SOURCES}
    ${app_utils_SCHEME_1}
    ${app_utils_SCHEME_1a}
    ${app_utils_SCHEME_1b}
    ${app_utils_SCHEME_1c}
    ${app_utils_SCHEME_2}
    ${app_utils_SCHEME_3}
    ${expressions_all_sources}
    CMakeLists.txt gnc-help-utils.c)


set(app_utils_DIST
    ${app_utils_DIST_local}
    ${test_app_utils_DIST}
    ${app_utils_mocks_DIST}
    PARENT_SCOPE)
