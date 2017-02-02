ADD_SUBDIRECTORY(test)

SET (report_gnome_HEADERS
  dialog-report-column-view.h
  dialog-custom-report.h
  dialog-report-style-sheet.h
  gnc-plugin-page-report.h
  window-report.h
)

IF (BUILDING_FROM_VCS)
  SET (SWIG_REPORT_GNOME_C ${CMAKE_CURRENT_BINARY_DIR}/swig-report-gnome.c)
  GNC_ADD_SWIG_COMMAND (swig-report-gnome-c ${SWIG_REPORT_GNOME_C} ${CMAKE_CURRENT_SOURCE_DIR}/report-gnome.i ${report_gnome_HEADERS})
ELSE()
  SET (SWIG_REPORT_GNOME_C swig-report-gnome.c)
ENDIF()

SET (report_gnome_SOURCES
  dialog-report-column-view.c
  dialog-custom-report.c
  dialog-report-style-sheet.c
  gnc-plugin-page-report.c
  gncmod-report-gnome.c
  window-report.c
)

ADD_LIBRARY (gncmod-report-gnome
  ${report_gnome_SOURCES}
  ${report_gnome_HEADERS}
  ${SWIG_REPORT_GNOME_C}
)
TARGET_LINK_LIBRARIES(gncmod-report-gnome gncmod-report-system gncmod-html gncmod-gnome-utils
      gnc-qof ${GUILE_LDFLAGS} ${GTK2_LDFLAGS})

TARGET_COMPILE_DEFINITIONS (gncmod-report-gnome PRIVATE -DG_LOG_DOMAIN=\"gnc.report.gui\")

TARGET_INCLUDE_DIRECTORIES(gncmod-report-gnome PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

INSTALL(TARGETS gncmod-report-gnome
  LIBRARY DESTINATION lib/gnucash
  ARCHIVE DESTINATION lib/gnucash
  RUNTIME DESTINATION bin)

INSTALL(FILES ${report_gnome_HEADERS} DESTINATION include/gnucash)

# Scheme

SET(report_gnome_SCHEME
    report-gnome.scm
)

SET(GUILE_OUTPUT_DIR   gnucash/report)
SET(GUILE_MODULES      src/app-utils src/scm src/engine src/gnc-module src/gnome-utils src/reports/report-system src/report/utility-reports)
SET(GUILE_LOAD_DIRS    src/app-utils src/gnc-module src/engine src/scm src/core-utils src/gnome-utils)
SET(GUILE_LIBRARY_DIRS src/app-utils src/core-utils src/engine src/gnc-module src/gnome-utils src/report-gnome)
SET(GUILE_DEPENDS      gncmod-report-gnome gncmod-engine scm-app-utils scm-gnome-utils scm-core-utils scm-gnc-module scm-scm)

GNC_ADD_SCHEME_TARGETS(scm-report-gnome
    "${report_gnome_SCHEME}"
    ${GUILE_OUTPUT_DIR}
    "${GUILE_MODULES}"
    "${GUILE_LOAD_DIRS}"
    "${GUILE_LIBRARY_DIRS}"
    "${GUILE_DEPENDS}"
    FALSE
)

SET(report_gnome_GLADE dialog-custom-report.glade dialog-report.glade)

INSTALL(FILES ${report_gnome_GLADE} DESTINATION share/gnucash/gtkbuilder)

SET(report_gnome_UI gnc-plugin-page-report-ui.xml)

INSTALL(FILES ${report_gnome_UI} DESTINATION share/gnucash/ui)

FILE(COPY ${report_gnome_UI} DESTINATION ${DATADIR_BUILD}/gnucash/ui)
FILE(COPY ${report_gnome_GLADE} DESTINATION ${DATADIR_BUILD}/gnucash/gtkbuilder)

SET_LOCAL_DIST(report_gnome_DIST_local CMakeLists.txt Makefile.am report-gnome.i ${report_gnome_HEADERS} ${report_gnome_SOURCES}
        ${report_gnome_SCHEME} ${report_gnome_GLADE} ${report_gnome_UI})
SET(report_gnome_DIST ${report_gnome_DIST_local} ${test_report_gnome_DIST} PARENT_SCOPE)