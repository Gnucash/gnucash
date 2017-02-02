ADD_SUBDIRECTORY(test)

SET(utility_reports_SOURCES
  gncmod-utility-reports.c
)

ADD_LIBRARY(gncmod-utility-reports MODULE ${utility_reports_SOURCES})

TARGET_LINK_LIBRARIES(gncmod-utility-reports gnc-module ${GUILE_LDFLAGS} ${GLIB2_LDFLAGS})

TARGET_COMPILE_DEFINITIONS(gncmod-utility-reports PRIVATE -DG_LOG_DOMAIN=\"gnc.report.util\")

TARGET_INCLUDE_DIRECTORIES(gncmod-utility-reports
    PRIVATE ${GUILE_INCLUDE_DIRS}
)

SET(LIB_DIR lib/gnucash)
IF (WIN32)
  SET(LIB_DIR bin)
ENDIF(WIN32)


INSTALL(TARGETS gncmod-utility-reports
  LIBRARY DESTINATION ${LIB_DIR}
  ARCHIVE DESTINATION ${LIB_DIR}
  RUNTIME DESTINATION bin)

# Scheme

SET(utility_reports_SCHEME
  hello-world.scm
  utility-reports.scm
  view-column.scm
  welcome-to-gnucash.scm
)

SET(GUILE_OUTPUT_DIR     gnucash/report)
SET(GUILE_MODULES        src/app-utils src/engine src/gnome-utils src/html src/report/report-system)
SET(GUILE_LOAD_DIRS      src/app-utils src/core-utils src/engine src/gnc-module
                            src/gnome-utils src/report/report-system src/scm)
SET(GUILE_LIBRARY_DIRS   src/engine src/app-utils src/core-utils src/gnc-module src/gnome-utils src/html
                              src/report/report-system)
SET(GUILE_DEPENDS        scm-utility-reports-links scm-app-utils scm-report-system-3)

GNC_ADD_SCHEME_TARGETS(scm-utility-reports
  "${utility_reports_SCHEME}"
  ${GUILE_OUTPUT_DIR}
  "${GUILE_MODULES}"
  "${GUILE_LOAD_DIRS}"
  "${GUILE_LIBRARY_DIRS}"
  "${GUILE_DEPENDS}"
  TRUE
)

SET_LOCAL_DIST(utility_reports_DIST_local CMakeLists.txt Makefile.am
        ${utility_reports_SOURCES} ${utility_reports_SCHEME})
SET(utility_reports_DIST ${utility_reports_DIST_local} ${test_utility_reports_DIST} PARENT_SCOPE)
