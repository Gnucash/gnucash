ADD_SUBDIRECTORY(test)

SET (report_system_HEADERS
  gnc-report.h
)

# Command to generate the swig-report-system.c wrapper file
gnc_add_swig_guile_command (swig-report-system-c
    SWIG_REPORT_SYSTEM_C swig-report-system.c
    ${CMAKE_CURRENT_SOURCE_DIR}/report-system.i ${report_system_HEADERS}
)

SET (report_system_SOURCES
  gncmod-report-system.c
  gnc-report.c
)  

ADD_LIBRARY (gncmod-report-system
  ${report_system_SOURCES}
  ${report_system_HEADERS}
  ${SWIG_REPORT_SYSTEM_C}
)

TARGET_COMPILE_DEFINITIONS(gncmod-report-system PRIVATE -DG_LOG_DOMAIN=\"gnc.report.core\")

TARGET_LINK_LIBRARIES(gncmod-report-system gnc-module gncmod-app-utils ${GTK3_LDFLAGS} ${GUILE_LDFLAGS})

TARGET_INCLUDE_DIRECTORIES (gncmod-report-system
    PUBLIC ${CMAKE_CURRENT_SOURCE_DIR} PRIVATE ${GTK3_INCLUDE_DIRS}
)


IF (APPLE)
  SET_TARGET_PROPERTIES (gncmod-report-system PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
ENDIF()

INSTALL(TARGETS gncmod-report-system
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

INSTALL(FILES ${report_system_HEADERS} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/gnucash)

# Scheme

SET (report_system_SCHEME
    report-system.scm
    eguile-gnc.scm
    eguile-utilities.scm
    eguile-html-utilities.scm
)

SET (report_system_SCHEME_2a
    collectors.scm
    list-extras.scm
)

SET (report_system_SCHEME_2b
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

SET(GUILE_DEPENDS
  gncmod-html
  scm-gnome-utils
  gncmod-report-system
  scm-app-utils
  scm-scm
  scm-gnc-module
  )

GNC_ADD_SCHEME_TARGETS(scm-report-system-1
  "${report_system_SCHEME}"
  "gnucash/report"
  "${GUILE_DEPENDS}"
  FALSE
)

GNC_ADD_SCHEME_TARGETS(scm-report-system-2a
  "${report_system_SCHEME_2a}"
  "gnucash/report/report-system"
  scm-report-system-1
  FALSE
)

GNC_ADD_SCHEME_TARGETS(scm-report-system-2b
  "${report_system_SCHEME_2b}"
  "gnucash/report/report-system"
  scm-report-system-2a
  FALSE
)

GNC_ADD_SCHEME_TARGETS(scm-report-system-3
  "${report_system_SCHEME_3}"
  ""
  scm-report-system-2b
  FALSE
)

add_custom_target(scm-report-system ALL DEPENDS scm-report-system-3)

SET_LOCAL_DIST(report_system_DIST_local CMakeLists.txt
  report-system.i
  ${report_system_HEADERS} ${report_system_SOURCES}
  ${report_system_SCHEME} ${report_system_SCHEME_1}
  ${report_system_SCHEME_2a} ${report_system_SCHEME_2b}
  ${report_system_SCHEME_3})

SET(report_system_DIST ${report_system_DIST_local} ${test_report_system_DIST} PARENT_SCOPE)
