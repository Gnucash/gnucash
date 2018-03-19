# NB: Unit tests which require GSchemas should be made conditional on COMPILE_GSCHEMAS.
ADD_SUBDIRECTORY(test)
# Build the library

SET (app_utils_noinst_HEADERS
  calculation/finvar.h
  calculation/finproto.h
  calculation/fin_spl_protos.h
  calculation/fin_static_proto.h
)

SET (app_utils_HEADERS
  QuickFill.h
  business-helpers.h
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

SET (app_utils_SOURCES
  calculation/expression_parser.c
  calculation/fin.c
  business-helpers.c
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

SET_SOURCE_FILES_PROPERTIES (${app_utils_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

SET(app_utils_ALL_SOURCES ${app_utils_SOURCES} ${app_utils_HEADERS} ${app_utils_noinst_HEADERS})
SET(app_utils_ALL_LIBRARIES gncmod-engine gnc-module ${GIO_LDFLAGS} ${LIBXML2_LDFLAGS} ${LIBXSLT_LDFLAGS})
SET(app_utils_ALL_INCLUDES  ${CMAKE_CURRENT_SOURCE_DIR}/calculation ${GIO_INCLUDE_DIRS} ${LIBXML2_INCLUDE_DIRS}
    ${LIBXSLT_INCLUDE_DIRS} ${CMAKE_SOURCE_DIR}/libgnucash/backend ${CMAKE_SOURCE_DIR}/libgnucash/engine)

IF (WIN32)
  LIST(APPEND app_utils_ALL_SOURCES gnc-help-utils.c)
  LIST(APPEND app_utils_ALL_LIBRARIES ${HTMLHELP_LIBRARY})
  LIST(APPEND app_utils_ALL_INCLUDES ${HTMLHELP_INCLUDE_PATH})
ENDIF()


ADD_LIBRARY (gncmod-app-utils ${app_utils_ALL_SOURCES} ${SWIG_APP_UTILS_GUILE_C})
TARGET_LINK_LIBRARIES(gncmod-app-utils ${app_utils_ALL_LIBRARIES})

TARGET_INCLUDE_DIRECTORIES (gncmod-app-utils
    PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}
    PRIVATE ${app_utils_ALL_INCLUDES}
)
TARGET_COMPILE_DEFINITIONS (gncmod-app-utils PRIVATE -DG_LOG_DOMAIN=\"gnc.app-utils\")

IF (APPLE)
  SET_TARGET_PROPERTIES (gncmod-app-utils PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
ENDIF()

INSTALL(TARGETS gncmod-app-utils
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)

IF (WITH_PYTHON)
  SET(PYEXEC_FILES sw_app_utils.py)
  ADD_LIBRARY (sw_app_utils MODULE ${SWIG_APP_UTILS_PYTHON_C})

  TARGET_LINK_LIBRARIES(sw_app_utils gncmod-app-utils ${app_utils_ALL_LIBRARIES} ${PYTHON_LIBRARIES})
  SET_TARGET_PROPERTIES(sw_app_utils PROPERTIES PREFIX "_")

  TARGET_INCLUDE_DIRECTORIES (sw_app_utils
    PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}
    PRIVATE ${app_utils_ALL_INCLUDES} ${PYTHON_INCLUDE_DIRS}
  )
  TARGET_COMPILE_DEFINITIONS (sw_app_utils PRIVATE -DG_LOG_DOMAIN=\"gnc.app-utils\")

  IF (APPLE)
    SET_TARGET_PROPERTIES (sw_app_utils PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
  ENDIF()

  INSTALL(TARGETS sw_app_utils
    LIBRARY DESTINATION ${PYTHON_SYSCONFIG_OUTPUT}/gnucash
    ARCHIVE DESTINATION ${PYTHON_SYSCONFIG_OUTPUT}/gnucash
  )

  ADD_CUSTOM_TARGET(sw-app-utils-py ALL
    COMMAND ${CMAKE_COMMAND} -E copy ${SWIG_APP_UTILS_PYTHON_PY} ${PYTHON_SYSCONFIG_BUILD}/gnucash
    DEPENDS ${SWIG_APP_UTILS_PYTHON_C})

  ADD_CUSTOM_TARGET(sw-app-utils-build ALL
    COMMAND ${CMAKE_COMMAND} -E copy ${LIBDIR_BUILD}/gnucash/_sw_app_utils${CMAKE_SHARED_MODULE_SUFFIX} ${PYTHON_SYSCONFIG_BUILD}/gnucash
    DEPENDS sw_app_utils)
    
ENDIF()



INSTALL(FILES ${app_utils_HEADERS} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/gnucash)

IF (WITH_OFX)
  FILE(READ ${CMAKE_SOURCE_DIR}/gnucash/import-export/ofx/migratable-prefs-ofx.xml MIGRATABLE_PREFS_OFX)
ENDIF()

IF (WITH_AQBANKING)
  FILE(READ ${CMAKE_SOURCE_DIR}/gnucash/import-export/aqb/migratable-prefs-aqbanking.xml MIGRATABLE_PREFS_AQBANKING)
ENDIF()

SET(MIGRATABLE_PREFS_XML ${DATADIR_BUILD}/gnucash/migratable-prefs.xml)
FILE(COPY make-prefs-migration-script.xsl DESTINATION ${DATADIR_BUILD}/gnucash)

CONFIGURE_FILE(migratable-prefs.xml.in ${MIGRATABLE_PREFS_XML})

INSTALL(FILES ${MIGRATABLE_PREFS_XML} DESTINATION  ${CMAKE_INSTALL_DATADIR}/gnucash)
INSTALL(FILES make-prefs-migration-script.xsl DESTINATION  ${CMAKE_INSTALL_DATADIR}/gnucash)

# And now handle scheme files

SET (app_utils_SCHEME_2
    app-utils.scm
)

SET (app_utils_SCHEME_1
    hooks.scm
    business-options.scm
    c-interface.scm
    config-var.scm
    date-utilities.scm
    migrate-prefs.scm
    options.scm
    prefs.scm
    simple-obj.scm
    fin.scm
)

SET(app_utils_SCHEME_3
    business-prefs.scm
)

SET(GUILE_DEPENDS
  gncmod-app-utils
  scm-core-utils
  scm-engine
  scm-scm
  scm-gnc-module
  )

GNC_ADD_SCHEME_TARGETS(scm-gettext
  gettext.scm
  gnucash
  gncmod-app-utils
  FALSE
)

GNC_ADD_SCHEME_TARGETS(scm-app-utils-1
  "${app_utils_SCHEME_1}"
  ""
  "scm-gettext;${GUILE_DEPENDS}"
  FALSE
)

GNC_ADD_SCHEME_TARGETS(scm-app-utils-2
  "${app_utils_SCHEME_2}"
  "gnucash"
  "scm-app-utils-1"
  FALSE
)

GNC_ADD_SCHEME_TARGETS(scm-app-utils-3
  "${app_utils_SCHEME_3}"
  ""
  "scm-app-utils-2"
  FALSE
)

ADD_CUSTOM_TARGET(scm-app-utils ALL DEPENDS scm-app-utils-3 scm-app-utils-2 scm-app-utils-1 scm-gettext)

SET_LOCAL_DIST(app_utils_DIST_local
        ${app_utils_ALL_SOURCES} gettext.scm ${app_utils_SCHEME_1} ${app_utils_SCHEME_2} ${app_utils_SCHEME_3}
        app-utils.i CMakeLists.txt gnc-help-utils.c 
        make-prefs-migration-script.xsl migratable-prefs.xml.in README)
SET(app_utils_DIST ${app_utils_DIST_local} ${test_app_utils_DIST} PARENT_SCOPE)
