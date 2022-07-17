# NB: Unit tests which require GSchemas should be made conditional on COMPILE_GSCHEMAS.
add_subdirectory(test)
add_subdirectory(mocks)

include (GncFindLibm)

# Build the library

set (app_utils_noinst_HEADERS
  calculation/finvar.h
  calculation/finproto.h
  calculation/fin_spl_protos.h
  calculation/fin_static_proto.h
  gnc-option-date.hpp
  gnc-option-impl.hpp
  gnc-option-ui.hpp
  gnc-option-uitype.hpp
  gnc-optiondb-impl.hpp
)

set (app_utils_HEADERS
  QuickFill.h
  file-utils.h
  gfec.h
  gnc-basic-gobject.h
  gnc-account-merge.h
  gnc-accounting-period.h
  gnc-addr-quickfill.h
  gnc-entry-quickfill.h
  gnc-euro.h
  gnc-exp-parser.h
  gnc-gsettings.h
  gnc-help-utils.h
  gnc-helpers.h
  gnc-option.hpp
  gnc-optiondb.h
  gnc-optiondb.hpp
  gnc-prefs-utils.h
  gnc-state.h
  gnc-sx-instance-model.h
  gnc-ui-util.h
  gnc-ui-balances.h
)

# Command to generate the swig-app-utils-guile.c wrapper file
set(SWIG_ARGS "-c++" "-procdoc" "sw-gnc-option-doc" "-procdocformat" "plain")
gnc_add_swig_guile_command (swig-apputils-guile-cpp #target
    SWIG_APP_UTILS_GUILE_CPP swig-app-utils-guile.cpp #outvar, output
    ${CMAKE_CURRENT_SOURCE_DIR}/app-utils.i #input
    "${CMAKE_SOURCE_DIR}/bindings;${CMAKE_SOURCE_DIR}/bindings/guile" #includes
    ${CMAKE_CURRENT_SOURCE_DIR}/gnc-optiondb.i #additional dependencies
)
unset(SWIG_ARGS)

# Command to generate the swig-app-utils-python.c wrapper file
gnc_add_swig_python_command (swig-app-utils-python
    SWIG_APP_UTILS_PYTHON_C SWIG_APP_UTILS_PYTHON_PY
    swig-app-utils-python.c sw_app_utils.py
    ${CMAKE_CURRENT_SOURCE_DIR}/app-utils.i ""
)

set (app_utils_SOURCES
  calculation/expression_parser.c
  calculation/fin.c
  QuickFill.c
  file-utils.c
  gfec.c
  gnc-account-merge.c
  gnc-accounting-period.c
  gnc-addr-quickfill.c
  gnc-entry-quickfill.c
  gnc-euro.c
  gnc-exp-parser.c
  gnc-gsettings.cpp
  gnc-helpers.c
  gnc-option-date.cpp
  gnc-option.cpp
  gnc-option-impl.cpp
  gnc-optiondb.cpp
  gnc-prefs-utils.c
  gnc-sx-instance-model.c
  gnc-state.c
  gnc-ui-util.c
  gnc-ui-balances.c
  )

set_source_files_properties (${app_utils_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

gnc_check_standard_math_library()
if (NOT STANDARD_MATH_LIBRARY_FOUND)
  message(FATAL_ERROR "An implementation of the standard C function pow() is required and is supported neither by the C runtime nor libm.so.")
endif()

set(app_utils_ALL_SOURCES ${app_utils_SOURCES} ${app_utils_HEADERS} ${app_utils_noinst_HEADERS})
set(app_utils_ALL_LIBRARIES
    gnc-engine
    gnc-locale-tax
    gnucash-guile
    ${GLIB_LDFLAGS}
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
  list(APPEND app_utils_ALL_LIBRARIES ${HTMLHELP_LIBRARY})
  list(APPEND app_utils_ALL_INCLUDES ${HTMLHELP_INCLUDE_PATH})
endif()


add_library (gnc-app-utils ${app_utils_ALL_SOURCES} ${SWIG_APP_UTILS_GUILE_CPP})
target_link_libraries(gnc-app-utils ${app_utils_ALL_LIBRARIES})

target_include_directories (gnc-app-utils
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}
        ${GLIB2_INCLUDE_DIRS}
    PRIVATE
        ${app_utils_ALL_INCLUDES}
)
target_compile_definitions (gnc-app-utils PRIVATE -DG_LOG_DOMAIN=\"gnc.app-utils\")

if (APPLE)
  set_target_properties (gnc-app-utils PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}")
endif()

install(TARGETS gnc-app-utils
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)

if (WITH_PYTHON)
  set(PYEXEC_FILES sw_app_utils.py)
  add_library (sw_app_utils MODULE ${SWIG_APP_UTILS_PYTHON_C})

  target_link_libraries(sw_app_utils gnc-app-utils ${app_utils_ALL_LIBRARIES} ${PYTHON_LIBRARIES})
  set_target_properties(sw_app_utils PROPERTIES PREFIX "_")
  if (HAVE_STRINGOP_TRUNCATION)
    target_compile_options(sw_app_utils PRIVATE -Wno-error=stringop-truncation)
  endif()

  target_include_directories (sw_app_utils
    PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}
    PRIVATE ${app_utils_ALL_INCLUDES} ${PYTHON_INCLUDE_DIRS}
  )
  target_compile_definitions (sw_app_utils PRIVATE -DG_LOG_DOMAIN=\"gnc.app-utils\")

  if (APPLE)
    set_target_properties (sw_app_utils PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}")
  endif()

  install(TARGETS sw_app_utils
    LIBRARY DESTINATION ${PYTHON_SYSCONFIG_OUTPUT}/gnucash
    ARCHIVE DESTINATION ${PYTHON_SYSCONFIG_OUTPUT}/gnucash
  )

  add_custom_target(sw-app-utils-py ALL
    COMMAND ${CMAKE_COMMAND} -E copy ${SWIG_APP_UTILS_PYTHON_PY} ${PYTHON_SYSCONFIG_BUILD}/gnucash
    DEPENDS ${SWIG_APP_UTILS_PYTHON_C})

  add_custom_target(sw-app-utils-build ALL
    COMMAND ${CMAKE_COMMAND} -E copy ${LIBDIR_BUILD}/gnucash/_sw_app_utils${CMAKE_SHARED_MODULE_SUFFIX} ${PYTHON_SYSCONFIG_BUILD}/gnucash
    DEPENDS sw_app_utils)

endif()



install(FILES ${app_utils_HEADERS} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/gnucash)

# And now handle scheme files

set (app_utils_SCHEME_1
    c-interface.scm
    date-utilities.scm
    fin.scm
)

set (app_utils_SCHEME_1a
    options.scm
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

gnc_add_scheme_targets(scm-app-utils-1a
    SOURCES "${app_utils_SCHEME_1a}"
    OUTPUT_DIR "gnucash/app-utils"
    DEPENDS "scm-app-utils-1"
    MAKE_LINKS)

gnc_add_scheme_targets(scm-app-utils-2
    SOURCES "${app_utils_SCHEME_2}"
    OUTPUT_DIR "gnucash"
    DEPENDS "scm-app-utils-1a"
    MAKE_LINKS)

add_custom_target(scm-app-utils ALL DEPENDS scm-app-utils-2 scm-app-utils-1)

set_local_dist(app_utils_DIST_local
    ${app_utils_ALL_SOURCES}
    ${app_utils_SCHEME_1}
    ${app_utils_SCHEME_1a}
    ${app_utils_SCHEME_1b}
    ${app_utils_SCHEME_1c}
    ${app_utils_SCHEME_2}
    ${app_utils_SCHEME_3}
    app-utils.i gnc-optiondb.i CMakeLists.txt gnc-help-utils.c)

set(app_utils_DIST
    ${app_utils_DIST_local}
    ${test_app_utils_DIST}
    ${app_utils_mocks_DIST}
    PARENT_SCOPE)
