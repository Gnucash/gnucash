#ADD_SUBDIRECTORY(test)

SET (report_system_HEADERS
  gnc-report.h
)  

SET (SWIG_REPORT_SYSTEM_C ${CMAKE_CURRENT_BINARY_DIR}/swig-report-system.c)
GNC_ADD_SWIG_COMMAND (${SWIG_REPORT_SYSTEM_C} ${CMAKE_CURRENT_SOURCE_DIR}/report-system.i ${report_system_HEADERS})

SET (report_system_SOURCES
  ${SWIG_REPORT_SYSTEM_C}
  gncmod-report-system.c
  gnc-report.c
)  

ADD_LIBRARY (gncmod-report-system
  ${report_system_SOURCES}
  ${report_system_HEADERS}
)

TARGET_COMPILE_DEFINITIONS(gncmod-report-system PRIVATE -DG_LOG_DOMAIN=\"gnc.report.core\")

TARGET_LINK_LIBRARIES(gncmod-report-system gnc-module gncmod-app-utils gnc-qof ${GTK2_LDFLAGS} ${GUILE_LDFLAGS})

TARGET_INCLUDE_DIRECTORIES (gncmod-report-system
    PUBLIC ${CMAKE_CURRENT_SOURCE_DIR} PRIVATE ${GTK2_INCLUDE_DIRS}
)


INSTALL(TARGETS gncmod-report-system
  LIBRARY DESTINATION lib/gnucash
  ARCHIVE DESTINATION lib/gnucash
  RUNTIME DESTINATION bin)

INSTALL(FILES ${report_system_HEADERS} DESTINATION include/gnucash)

# Scheme

SET (report_system_SCHEME
    report-system.scm
    eguile-gnc.scm
    eguile-utilities.scm
    eguile-html-utilities.scm
)

SET (report_system_SCHEME_2
    collectors.scm
    list-extras.scm
    report-collectors.scm
)

SET (report_system_SCHEME_3
    commodity-utilities.scm
    html-acct-table.scm
    html-barchart.scm
    html-document.scm
    html-fonts.scm
    html-piechart.scm
    html-scatter.scm
    html-linechart.scm
    html-style-info.scm
    html-style-sheet.scm
    html-table.scm
    html-text.scm
    html-utilities.scm
    html-jqplot.scm
    options-utilities.scm
    report-utilities.scm
    report.scm
)


SET(GUILE_MODULES      src/app-utils src/engine src/html src/gnome-utils)
SET(GUILE_LOAD_DIRS    src/app-utils src/core-utils src/engine src/gnc-module src/gnome-utils src/report/report-system src/scm)
SET(GUILE_LIBRARY_DIRS src/app-utils src/core-utils src/engine src/gnc-module src/gnome-utils)
SET(GUILE_DEPENDS      gncmod-html gncmod-gnome-utils gncmod-report-system scm-app-utils scm-gnome-utils)


GNC_ADD_SCHEME_TARGETS(scm-report-system
  "${report_system_SCHEME}"
  "gnucash/report"
  "${GUILE_MODULES}"
  "${GUILE_LOAD_DIRS}"
  "${GUILE_LIBRARY_DIRS}"
  "${GUILE_DEPENDS}"
  FALSE
)

GNC_ADD_SCHEME_TARGETS(scm-report-system-2
  "${report_system_SCHEME_2}"
  "gnucash/report/report-system"
  "${GUILE_MODULES}"
  "${GUILE_LOAD_DIRS}"
  "${GUILE_LIBRARY_DIRS}"
  scm-report-system
  FALSE
)

GNC_ADD_SCHEME_TARGETS(scm-report-system-3
  "${report_system_SCHEME_3}"
  ""
  "${GUILE_MODULES}"
  "${GUILE_LOAD_DIRS}"
  "${GUILE_LIBRARY_DIRS}"
  scm-report-system-2
  FALSE
)

