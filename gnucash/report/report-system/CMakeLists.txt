add_subdirectory(test)

set (report_system_HEADERS
  gnc-report.h
)

# Command to generate the swig-report-system.c wrapper file
gnc_add_swig_guile_command (swig-report-system-c
    SWIG_REPORT_SYSTEM_C swig-report-system.c
    ${CMAKE_CURRENT_SOURCE_DIR}/report-system.i ${report_system_HEADERS}
)

set (report_system_SOURCES
  gncmod-report-system.c
  gnc-report.c
)  

add_library (gncmod-report-system
  ${report_system_SOURCES}
  ${report_system_HEADERS}
  ${SWIG_REPORT_SYSTEM_C}
)

target_compile_definitions(gncmod-report-system PRIVATE -DG_LOG_DOMAIN=\"gnc.report.core\")

target_link_libraries(gncmod-report-system gnc-module gncmod-app-utils PkgConfig::GTK3 ${GUILE_LDFLAGS})

target_include_directories (gncmod-report-system
    PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}
)


if (APPLE)
  set_target_properties (gncmod-report-system PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
endif()

install(TARGETS gncmod-report-system
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

install(FILES ${report_system_HEADERS} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/gnucash)

# Scheme

set (report_system_SCHEME
    report-system.scm
    eguile-gnc.scm
)

set (report_system_SCHEME_1
    eguile-utilities.scm
    eguile-html-utilities.scm
)

set (report_system_SCHEME_2a
    collectors.scm
)

set (report_system_SCHEME_2b
    report-collectors.scm
)

set (report_system_SCHEME_3
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
    html-anytag.scm
    html-table.scm
    html-text.scm
    html-utilities.scm
    options-utilities.scm
    report-utilities.scm
    report.scm
    trep-engine.scm
)

set(GUILE_DEPENDS
  gncmod-html
  scm-gnome-utils
  gncmod-report-system
  scm-app-utils
  scm-scm
  scm-gnc-module
  )

gnc_add_scheme_targets(scm-report-system-1
  "${report_system_SCHEME}"
  "gnucash/report"
  "${GUILE_DEPENDS}"
  FALSE
)

gnc_add_scheme_targets(scm-report-system-1a
  "${report_system_SCHEME_1}"
  "gnucash/report"
  scm-report-system-1
  FALSE
)

gnc_add_scheme_targets(scm-report-system-2a
  "${report_system_SCHEME_2a}"
  "gnucash/report/report-system"
  scm-report-system-1a
  FALSE
)

gnc_add_scheme_targets(scm-report-system-2b
  "${report_system_SCHEME_2b}"
  "gnucash/report/report-system"
  scm-report-system-2a
  FALSE
)

gnc_add_scheme_targets(scm-report-system-3
  "${report_system_SCHEME_3}"
  ""
  scm-report-system-2b
  FALSE
)

add_custom_target(scm-report-system ALL DEPENDS scm-report-system-3)

set_local_dist(report_system_DIST_local CMakeLists.txt
  report-system.i
  ${report_system_HEADERS} ${report_system_SOURCES}
  ${report_system_SCHEME} ${report_system_SCHEME_1}
  ${report_system_SCHEME_2a} ${report_system_SCHEME_2b}
  ${report_system_SCHEME_3})

set(report_system_DIST ${report_system_DIST_local} ${test_report_system_DIST} PARENT_SCOPE)
