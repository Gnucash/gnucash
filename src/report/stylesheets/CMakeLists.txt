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
    gnc-qof ${GUILE_LDFLAGS} ${GTK2_LDFLAGS} ${GLIB2_LDFLAGS})

TARGET_COMPILE_DEFINITIONS(gncmod-stylesheets PRIVATE -DG_LOG_DOMAIN=\"gnc.report.core\")

SET(LIB_DIR lib/gnucash)
IF (WIN32)
  SET(LIB_DIR bin)
ENDIF(WIN32)


INSTALL(TARGETS gncmod-stylesheets
  LIBRARY DESTINATION ${LIB_DIR}
  ARCHIVE DESTINATION ${LIB_DIR}
  RUNTIME DESTINATION bin)

# No headers to install

# Scheme

SET(stylesheets_SCHEME_1
  stylesheet-plain.scm
  stylesheet-fancy.scm
  stylesheet-footer.scm
  stylesheet-easy.scm
)

SET(stylesheets_SCHEME_2
  stylesheets.scm
)

SET(GUILE_OUTPUT_DIR gnucash/report)
SET(GUILE_MODULES src/app-utils src/engine src/html src/gnome-utils src/report/report-system)
SET(GUILE_LOAD_DIRS src/app-utils src/core-utils src/engine src/gnc-module src/gnome-utils src/report/report-system src/scm)
SET(GUILE_LIBRARY_DIRS src/engine src/app-utils src/core-utils src/gnc-module src/gnome-utils src/html src/report/report-system)
SET(GUILE_DEPENDS gncmod-html scm-app-utils scm-report-system-3)

GNC_ADD_SCHEME_TARGETS(scm-report-stylesheets-1
  "${stylesheets_SCHEME_1}"
  ${GUILE_OUTPUT_DIR}
  "${GUILE_MODULES}"
  "${GUILE_LOAD_DIRS}"
  "${GUILE_LIBRARY_DIRS}"
  "${GUILE_DEPENDS}"
  TRUE
)

GNC_ADD_SCHEME_TARGETS(scm-report-stylesheets-2
  "${stylesheets_SCHEME_2}"
  ${GUILE_OUTPUT_DIR}
  "${GUILE_MODULES}"
  "${GUILE_LOAD_DIRS}"
  "${GUILE_LIBRARY_DIRS}"
  "scm-report-stylesheets-1;${GUILE_DEPENDS}"
  TRUE
)

ADD_CUSTOM_TARGET(scm-report-stylesheets ALL DEPENDS scm-report-stylesheets-1 scm-report-stylesheets-2)

SET(stylesheets_UI gnc-plugin-stylesheets-ui.xml)

INSTALL(FILES ${stylesheets_UI} DESTINATION share/gnucash/ui)

FILE(COPY ${stylesheets_UI}
  DESTINATION ${DATADIR_BUILD}/gnucash/ui)

SET_LOCAL_DIST(stylesheets_DIST_local CMakeLists.txt Makefile.am
        ${stylesheets_SOURCES} ${stylesheets_noinst_HEADERS}
        ${stylesheets_SCHEME_1} ${stylesheets_SCHEME_2}  ${stylesheets_UI})

SET(stylesheets_DIST ${stylesheets_DIST_local} ${test_stylesheets_DIST} PARENT_SCOPE)
