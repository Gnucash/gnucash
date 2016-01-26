#ADD_SUBDIRECTORY(test)
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

# Command to generate the swig-app-utils.c wrapper file
SET (SWIG_APP_UTILS_C ${CMAKE_CURRENT_BINARY_DIR}/swig-app-utils.c)
GNC_ADD_SWIG_COMMAND (${SWIG_APP_UTILS_C} ${CMAKE_CURRENT_SOURCE_DIR}/app-utils.i)

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
  ${SWIG_APP_UTILS_C}
)

IF (WIN32)
  SET (app_utils_SOURCES ${app_utils_SOURCES} gnc-help-utils.c)
ENDIF (WIN32)

SET_SOURCE_FILES_PROPERTIES (${app_utils_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

ADD_LIBRARY	(gncmod-app-utils
  ${app_utils_SOURCES}
  ${app_utils_HEADERS}
  ${app_utils_noinst_HEADERS}
)

TARGET_LINK_LIBRARIES(gncmod-app-utils gncmod-engine gnc-module 
  ${GTK2_LDFLAGS} ${LIBXML2_LDFLAGS} ${LIBXSLT_LDFLAGS}
)

IF (WIN32) 
   TARGET_LINK_LIBRARIES(gncmod-app-utils ${HTMLHELP_LIBRARY})
ENDIF(WIN32)

TARGET_INCLUDE_DIRECTORIES (gncmod-app-utils
    PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}
    PRIVATE
      ${CMAKE_CURRENT_SOURCE_DIR}/calculation
      ${LIBXML2_INCLUDE_DIRS}
      ${LIBXSLT_INCLUDE_DIRS}
      ${HTMLHELP_INCLUDE_PATH}
      ${GTK2_INCLUDE_DIRS}
)

TARGET_COMPILE_DEFINITIONS (gncmod-app-utils PRIVATE -DG_LOG_DOMAIN=\"gnc.app-utils\")


INSTALL(TARGETS gncmod-app-utils
     LIBRARY DESTINATION lib/gnucash
     ARCHIVE DESTINATION lib/gnucash
     RUNTIME DESTINATION bin
)
INSTALL(FILES ${app_utils_HEADERS} DESTINATION include/gnucash)

FILE(READ ${CMAKE_SOURCE_DIR}/src/import-export/ofx/migratable-prefs-ofx.xml MIGRATABLE_PREFS_OFX)
FILE(READ ${CMAKE_SOURCE_DIR}/src/import-export/aqb/migratable-prefs-aqbanking.xml MIGRATABLE_PREFS_AQBANKING)

SET(MIGRATABLE_PREFS_XML ${CMAKE_CURRENT_BINARY_DIR}/migratable-prefs.xml)
IF (GNC_BUILD_AS_INSTALL)
  SET(MIGRATABLE_PREFS_XML ${DATADIR_BUILD}/gnucash/migratable-prefs.xml)
  FILE(COPY make-prefs-migration-script.xsl DESTINATION ${DATADIR_BUILD}/gnucash)
ENDIF()

CONFIGURE_FILE(migratable-prefs.xml.in ${MIGRATABLE_PREFS_XML})

INSTALL(FILES ${MIGRATABLE_PREFS_XML} DESTINATION share/gnucash)
INSTALL(FILES make-prefs-migration-script.xsl DESTINATION share/gnucash)

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
)

SET(app_utils_SCHEME_3
    business-prefs.scm
)

SET(GUILE_MODULES      src/engine)
SET(GUILE_LOAD_DIRS    src/app-utils src/core-utils src/engine src/gnc-module src/scm)
SET(GUILE_LIBRARY_DIRS src/app-utils src/core-utils src/engine src/gnc-module src/libqof/qof)
SET(GUILE_DEPENDS      gncmod-app-utils scm-core-utils scm-engine scm-scm)

GNC_ADD_SCHEME_TARGETS(scm-gettext
  gettext.scm
  gnucash
  src/app-utils
  ""
  ""
  gncmod-app-utils
  FALSE
)

GNC_ADD_SCHEME_TARGETS(scm-app-utils-1
  "${app_utils_SCHEME_1}"
  ""
  "${GUILE_MODULES}"
  "${GUILE_LOAD_DIRS}"
  "${GUILE_LIBRARY_DIRS}"
  "scm-gettext;${GUILE_DEPENDS}"
  FALSE
)

GNC_ADD_SCHEME_TARGETS(scm-app-utils-2
  "${app_utils_SCHEME_2}"
  "gnucash"
  "${GUILE_MODULES}"
  "${GUILE_LOAD_DIRS}"
  "${GUILE_LIBRARY_DIRS}"
  scm-app-utils-1
  FALSE
)

GNC_ADD_SCHEME_TARGETS(scm-app-utils-3
  "${app_utils_SCHEME_3}"
  ""
  "${GUILE_MODULES}"
  "${GUILE_LOAD_DIRS}"
  "${GUILE_LIBRARY_DIRS}"
  scm-app-utils-2
  FALSE
)

ADD_CUSTOM_TARGET(scm-app-utils ALL DEPENDS scm-app-utils-3 scm-app-utils-2 scm-app-utils-1 scm-gettext)
