ADD_SUBDIRECTORY(test)

SET(stylesheets_SOURCES
  gnc-plugin-stylesheets.c
  gncmod-stylesheets.c
)

SET(stylesheets_noinst_HEADERS
  gnc-plugin-stylesheets.h
)

ADD_LIBRARY(gncmod-stylesheets MODULE ${stylesheets_SOURCES} ${stylesheets_noinst_HEADERS})

TARGET_LINK_LIBRARIES(gncmod-stylesheets gncmod-report-gnome gncmod-report-system gncmod-gnome-utils gnc-module
    ${GUILE_LDFLAGS} ${GTK3_LDFLAGS} ${GLIB2_LDFLAGS})

TARGET_COMPILE_DEFINITIONS(gncmod-stylesheets PRIVATE -DG_LOG_DOMAIN=\"gnc.report.core\")

SET(LIB_DIR ${CMAKE_INSTALL_LIBDIR}/gnucash)
IF (WIN32)
  SET(LIB_DIR ${CMAKE_INSTALL_BINDIR})
ENDIF(WIN32)


IF (APPLE)
  SET_TARGET_PROPERTIES (gncmod-stylesheets PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
ENDIF()

INSTALL(TARGETS gncmod-stylesheets
  LIBRARY DESTINATION ${LIB_DIR}
  ARCHIVE DESTINATION ${LIB_DIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

# No headers to install

# Scheme

SET(stylesheets_SCHEME_1
  stylesheet-plain.scm
  stylesheet-fancy.scm
  stylesheet-footer.scm
  stylesheet-easy.scm
  stylesheet-head-or-tail.scm
)

SET(stylesheets_SCHEME_2
  stylesheets.scm
)

SET(GUILE_OUTPUT_DIR gnucash/report)
SET(GUILE_DEPENDS
  scm-gnc-module
  scm-core-utils
  scm-gettext
  scm-scm
  gncmod-html
  scm-report-system
  )

GNC_ADD_SCHEME_TARGETS(scm-report-stylesheets-1
  "${stylesheets_SCHEME_1}"
  ${GUILE_OUTPUT_DIR}
  "${GUILE_DEPENDS}"
  TRUE
)

GNC_ADD_SCHEME_TARGETS(scm-report-stylesheets-2
  "${stylesheets_SCHEME_2}"
  ${GUILE_OUTPUT_DIR}
  "scm-report-stylesheets-1;${GUILE_DEPENDS}"
  TRUE
)

ADD_CUSTOM_TARGET(scm-report-stylesheets ALL DEPENDS scm-report-stylesheets-1 scm-report-stylesheets-2)

SET_LOCAL_DIST(stylesheets_DIST_local CMakeLists.txt 
        ${stylesheets_SOURCES} ${stylesheets_noinst_HEADERS}
        ${stylesheets_SCHEME_1} ${stylesheets_SCHEME_2})

SET(stylesheets_DIST ${stylesheets_DIST_local} ${test_stylesheets_DIST} PARENT_SCOPE)
