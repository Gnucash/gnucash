
add_subdirectory(jqplot)
add_subdirectory(reports)
add_subdirectory(stylesheets)
add_subdirectory(test)

set (report_HEADERS
  gnc-report.h
)

# Command to generate the swig-report.c wrapper file
gnc_add_swig_guile_command (swig-report-c
    SWIG_REPORT_C swig-report.c
    ${CMAKE_CURRENT_SOURCE_DIR}/report.i ${report_HEADERS}
)

set (report_SOURCES
  gncmod-report.c
  gnc-report.c
)

add_library (gncmod-report
  ${report_SOURCES}
  ${report_HEADERS}
  ${SWIG_REPORT_C}
)

target_compile_definitions(gncmod-report PRIVATE -DG_LOG_DOMAIN=\"gnc.report.core\")

target_link_libraries(gncmod-report gnc-module gncmod-app-utils ${GTK3_LDFLAGS} ${GUILE_LDFLAGS})

target_include_directories (gncmod-report
    PUBLIC ${CMAKE_CURRENT_SOURCE_DIR} PRIVATE ${GTK3_INCLUDE_DIRS}
)


if (APPLE)
  set_target_properties (gncmod-report PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
endif()

install(TARGETS gncmod-report
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

install(FILES ${report_HEADERS} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/gnucash)

# Scheme

set (report_SCHEME
    report.scm
)

set (report_SCHEME_2
    commodity-utilities.scm
    html-acct-table.scm
    html-chart.scm
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
    report-core.scm
    report-utilities.scm
    report-register-hooks.scm
    trep-engine.scm
)

set (report_eguile_parts_SCHEME
    eguile-utilities.scm
    eguile-html-utilities.scm
)

set (report_eguile_SCHEME
    eguile.scm
)

set(GUILE_DEPENDS
  gncmod-html
  scm-gnome-utils
  gncmod-report
  scm-app-utils
  scm-scm
  scm-gnc-module
  )

gnc_add_scheme_targets(scm-report-1
  "${report_SCHEME}"
  "gnucash"
  "${GUILE_DEPENDS}"
  FALSE
)

gnc_add_scheme_targets(scm-report-2
  "${report_SCHEME_2}"
  "gnucash/report"
  scm-report-1
  TRUE
)

gnc_add_scheme_targets(scm-report-eguile-parts
  "${report_eguile_parts_SCHEME}"
  "gnucash/eguile"
  scm-report-1
  FALSE
)

gnc_add_scheme_targets(scm-report-eguile
  "${report_eguile_SCHEME}"
  "gnucash"
  scm-report-eguile-parts
  FALSE
)

# Module interfaces deprecated in 4.x, will be removed for 5.x
gnc_add_scheme_deprecated_module ("gnucash report eguile-gnc" "gnucash eguile" "scm-report-eguile" "")
gnc_add_scheme_deprecated_module ("gnucash report eguile-html-utilities" "gnucash eguile" "scm-report-eguile" "")
gnc_add_scheme_deprecated_module ("gnucash report eguile-utilities" "gnucash eguile" "scm-report-eguile" "")
gnc_add_scheme_deprecated_module ("gnucash report report-system" "gnucash report" "scm-report-1" "")

gnc_add_scheme_deprecated_module ("gnucash report business-reports" "" "" "")
gnc_add_scheme_deprecated_module ("gnucash report report-system collectors" "" "" "")
gnc_add_scheme_deprecated_module ("gnucash report report-system report-collectors" "" "" "")
gnc_add_scheme_deprecated_module ("gnucash report stylesheets" "" "" "")
gnc_add_scheme_deprecated_module ("gnucash report utility-reports" "" "" "")

add_custom_target(scm-report ALL DEPENDS scm-report-eguile)

set_local_dist(report_DIST_local CMakeLists.txt
  report.i
  ${report_HEADERS} ${report_SOURCES}
  ${report_SCHEME} ${report_SCHEME_1} ${report_SCHEME_2}
  ${report_eguile_parts_SCHEME} ${report_eguile_SCHEME})

set(report_DIST ${report_DIST_local} ${jqplot_DIST}
                ${reports_DIST} ${stylesheets_DIST} ${test_report_DIST} PARENT_SCOPE)
