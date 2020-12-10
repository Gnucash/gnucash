
add_subdirectory(reports)
add_subdirectory(stylesheets)
add_subdirectory(test)

set (report_HEADERS
  gnc-report.h
)

# Command to generate the swig-report.c wrapper file
gnc_add_swig_guile_command (swig-report-c
    SWIG_REPORT_C swig-report.c
    ${CMAKE_CURRENT_SOURCE_DIR}/report.i "" ${report_HEADERS}
)

set (report_SOURCES
  gnc-report.c
)

add_library (gnc-report
  ${report_SOURCES}
  ${report_HEADERS}
  ${SWIG_REPORT_C}
)

target_compile_definitions(gnc-report PRIVATE -DG_LOG_DOMAIN=\"gnc.report.core\")

target_link_libraries(gnc-report
    gnc-app-utils
    gnucash-guile
    PkgConfig::GTK3
    ${GUILE_LDFLAGS})

target_include_directories (gnc-report
    PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}
)


if (APPLE)
  set_target_properties (gnc-report PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
endif()

install(TARGETS gnc-report
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

install(FILES ${report_HEADERS} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/gnucash)

# Scheme

set (report_SCHEME_1
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

set (report_SCHEME_2
    report.scm
)

set (report_eguile_parts_SCHEME
    eguile-utilities.scm
    eguile-html-utilities.scm
)

set (report_eguile_SCHEME
    eguile.scm
)

set(GUILE_DEPENDS
  scm-gnc-html
  scm-gnome-utils
  gnc-report
  scm-app-utils
  scm-engine
  guile-json
  )

gnc_add_scheme_targets(scm-report-1
    SOURCES "${report_SCHEME_1}"
    OUTPUT_DIR "gnucash/report"
    DEPENDS "${GUILE_DEPENDS}"
    MAKE_LINKS)

gnc_add_scheme_targets(scm-report-2
    SOURCES "${report_SCHEME_2}"
    OUTPUT_DIR "gnucash"
    DEPENDS scm-report-1
    MAKE_LINKS)

gnc_add_scheme_targets(scm-report-eguile-parts
    SOURCES "${report_eguile_parts_SCHEME}"
    OUTPUT_DIR "gnucash/eguile"
    DEPENDS scm-report-2)

gnc_add_scheme_targets(scm-report-eguile
    SOURCES "${report_eguile_SCHEME}"
    OUTPUT_DIR "gnucash"
    DEPENDS scm-report-eguile-parts)

# Module interfaces deprecated in 4.x, will be removed for 5.x
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report eguile-gnc"
    NEW_MODULE "gnucash eguile"
    DEPENDS "scm-report-eguile")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report eguile-html-utilities"
    NEW_MODULE "gnucash eguile"
    DEPENDS "scm-report-eguile")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report eguile-utilities"
    NEW_MODULE "gnucash eguile"
    DEPENDS "scm-report-eguile")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report report-system"
    NEW_MODULE "gnucash report"
    DEPENDS "scm-report-2")

gnc_add_scheme_deprecated_module (OLD_MODULE "gnucash report business-reports")
gnc_add_scheme_deprecated_module (OLD_MODULE "gnucash report report-system collectors")
gnc_add_scheme_deprecated_module (OLD_MODULE "gnucash report report-system report-collectors")
gnc_add_scheme_deprecated_module (OLD_MODULE "gnucash report stylesheets")
gnc_add_scheme_deprecated_module (OLD_MODULE "gnucash report utility-reports")

add_custom_target(scm-report ALL DEPENDS scm-report-2 scm-report-eguile)

set_local_dist(report_DIST_local CMakeLists.txt
  report.i
  ${report_HEADERS} ${report_SOURCES}
  ${report_SCHEME} ${report_SCHEME_1} ${report_SCHEME_2}
  ${report_eguile_parts_SCHEME} ${report_eguile_SCHEME})

set(report_DIST ${report_DIST_local}
                ${reports_DIST} ${stylesheets_DIST} ${test_report_DIST} PARENT_SCOPE)
