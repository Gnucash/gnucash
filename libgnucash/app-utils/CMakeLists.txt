# NB: Unit tests which require GSchemas should be made conditional on COMPILE_GSCHEMAS.
add_subdirectory(test)
# Build the library

set (app_utils_noinst_HEADERS
  calculation/finvar.h
  calculation/finproto.h
  calculation/fin_spl_protos.h
  calculation/fin_static_proto.h
)

set (app_utils_HEADERS
  QuickFill.h
  business-options.h
  file-utils.h
  gfec.h
  gnc-basic-gobject.h
  gnc-account-merge.h
  gnc-accounting-period.h
  gnc-addr-quickfill.h
  gnc-component-manager.h
  gnc-entry-quickfill.h
  gnc-euro.h
  gnc-exp-parser.h
  gnc-gettext-util.h
  gnc-gsettings.h
  gnc-help-utils.h
  gnc-helpers.h
  gnc-prefs-utils.h
  gnc-state.h  
  gnc-sx-instance-model.h
  gnc-ui-util.h
  gnc-ui-balances.h
  guile-util.h
  option-util.h
)

# Command to generate the swig-app-utils-guile.c wrapper file
gnc_add_swig_guile_command (swig-apputils-guile-c
    SWIG_APP_UTILS_GUILE_C swig-app-utils-guile.c
    ${CMAKE_CURRENT_SOURCE_DIR}/app-utils.i
)

# Command to generate the swig-app-utils-python.c wrapper file
gnc_add_swig_python_command (swig-app-utils-python
    SWIG_APP_UTILS_PYTHON_C SWIG_APP_UTILS_PYTHON_PY
    swig-app-utils-python.c sw_app_utils.py
    ${CMAKE_CURRENT_SOURCE_DIR}/app-utils.i
)

set (app_utils_SOURCES
  calculation/expression_parser.c
  calculation/fin.c
  business-options.c
  QuickFill.c
  file-utils.c
  gfec.c
  gnc-account-merge.c
  gnc-accounting-period.c
  gnc-addr-quickfill.c
  gnc-component-manager.c
  gnc-entry-quickfill.c
  gnc-euro.c
  gnc-exp-parser.c
  gnc-gettext-util.c
  gnc-gsettings.c
  gnc-helpers.c
  gnc-prefs-utils.c
  gnc-sx-instance-model.c
  gnc-state.c
  gnc-ui-util.c
  gnc-ui-balances.c
  gncmod-app-utils.c
  guile-util.c
  option-util.c
)

set_source_files_properties (${app_utils_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

set(app_utils_ALL_SOURCES ${app_utils_SOURCES} ${app_utils_HEADERS} ${app_utils_noinst_HEADERS})
set(app_utils_ALL_LIBRARIES gncmod-engine gnc-module ${GIO_LDFLAGS} ${LIBXML2_LDFLAGS} ${LIBXSLT_LDFLAGS})
set(app_utils_ALL_INCLUDES  ${CMAKE_CURRENT_SOURCE_DIR}/calculation ${GIO_INCLUDE_DIRS} ${LIBXML2_INCLUDE_DIRS}
    ${LIBXSLT_INCLUDE_DIRS} ${CMAKE_SOURCE_DIR}/libgnucash/backend ${CMAKE_SOURCE_DIR}/libgnucash/engine)

if (WIN32)
  list(APPEND app_utils_ALL_SOURCES gnc-help-utils.c)
  list(APPEND app_utils_ALL_LIBRARIES ${HTMLHELP_LIBRARY})
  list(APPEND app_utils_ALL_INCLUDES ${HTMLHELP_INCLUDE_PATH})
endif()


add_library (gncmod-app-utils ${app_utils_ALL_SOURCES} ${SWIG_APP_UTILS_GUILE_C})
target_link_libraries(gncmod-app-utils ${app_utils_ALL_LIBRARIES})

target_include_directories (gncmod-app-utils
    PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}
    PRIVATE ${app_utils_ALL_INCLUDES}
)
target_compile_definitions (gncmod-app-utils PRIVATE -DG_LOG_DOMAIN=\"gnc.app-utils\")

if (APPLE)
  set_target_properties (gncmod-app-utils PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
endif()

install(TARGETS gncmod-app-utils
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)

if (WITH_PYTHON)
  set(PYEXEC_FILES sw_app_utils.py)
  add_library (sw_app_utils MODULE ${SWIG_APP_UTILS_PYTHON_C})

  target_link_libraries(sw_app_utils gncmod-app-utils ${app_utils_ALL_LIBRARIES} ${PYTHON_LIBRARIES})
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
    set_target_properties (sw_app_utils PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
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

if (WITH_OFX)
  file(READ ${CMAKE_SOURCE_DIR}/gnucash/import-export/ofx/migratable-prefs-ofx.xml MIGRATABLE_PREFS_OFX)
endif()

if (WITH_AQBANKING)
  file(READ ${CMAKE_SOURCE_DIR}/gnucash/import-export/aqb/migratable-prefs-aqbanking.xml MIGRATABLE_PREFS_AQBANKING)
endif()

set(MIGRATABLE_PREFS_XML ${DATADIR_BUILD}/gnucash/migratable-prefs.xml)
file(COPY make-prefs-migration-script.xsl DESTINATION ${DATADIR_BUILD}/gnucash)

configure_file(migratable-prefs.xml.in ${MIGRATABLE_PREFS_XML})

install(FILES ${MIGRATABLE_PREFS_XML} DESTINATION  ${CMAKE_INSTALL_DATADIR}/gnucash)
install(FILES make-prefs-migration-script.xsl DESTINATION  ${CMAKE_INSTALL_DATADIR}/gnucash)

# And now handle scheme files

set (app_utils_SCHEME_2
    app-utils.scm
)

set (app_utils_SCHEME_1
    hooks.scm
    business-options.scm
    c-interface.scm
    date-utilities.scm
    migrate-prefs.scm
    options.scm
    prefs.scm
    simple-obj.scm
    fin.scm
)

set(app_utils_SCHEME_3
    business-prefs.scm
)

set(GUILE_DEPENDS
  gncmod-app-utils
  scm-core-utils
  scm-engine
  scm-scm
  scm-gnc-module
  )

gnc_add_scheme_targets(scm-gettext
  gettext.scm
  gnucash
  gncmod-app-utils
  FALSE
)

gnc_add_scheme_targets(scm-app-utils-1
  "${app_utils_SCHEME_1}"
  ""
  "scm-gettext;${GUILE_DEPENDS}"
  FALSE
)

gnc_add_scheme_targets(scm-app-utils-2
  "${app_utils_SCHEME_2}"
  "gnucash"
  "scm-app-utils-1"
  FALSE
)

gnc_add_scheme_targets(scm-app-utils-3
  "${app_utils_SCHEME_3}"
  ""
  "scm-app-utils-2"
  FALSE
)

add_custom_target(scm-app-utils ALL DEPENDS scm-app-utils-3 scm-app-utils-2 scm-app-utils-1 scm-gettext)

set_local_dist(app_utils_DIST_local
        ${app_utils_ALL_SOURCES} gettext.scm ${app_utils_SCHEME_1} ${app_utils_SCHEME_2} ${app_utils_SCHEME_3}
        app-utils.i CMakeLists.txt gnc-help-utils.c 
        make-prefs-migration-script.xsl migratable-prefs.xml.in README)
set(app_utils_DIST ${app_utils_DIST_local} ${test_app_utils_DIST} PARENT_SCOPE)
