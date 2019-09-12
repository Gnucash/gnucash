add_subdirectory(test)

set (report_gnome_HEADERS
  dialog-report-column-view.h
  dialog-custom-report.h
  dialog-report-style-sheet.h
  gnc-plugin-page-report.h
  window-report.h
)

# Command to generate the swig-report-gnome.c wrapper file
gnc_add_swig_guile_command (swig-report-gnome-c
    SWIG_REPORT_GNOME_C swig-report-gnome.c
    ${CMAKE_CURRENT_SOURCE_DIR}/report-gnome.i ${report_gnome_HEADERS}
)

set (report_gnome_SOURCES
  dialog-report-column-view.c
  dialog-custom-report.c
  dialog-report-style-sheet.c
  gnc-plugin-page-report.c
  gncmod-report-gnome.c
  window-report.c
)

add_library (gncmod-report-gnome
  ${report_gnome_SOURCES}
  ${report_gnome_HEADERS}
  ${SWIG_REPORT_GNOME_C}
)
target_link_libraries(gncmod-report-gnome gncmod-report-system gncmod-html gncmod-gnome-utils
     PkgConfig::GTK3 ${GUILE_LDFLAGS} )

target_compile_definitions (gncmod-report-gnome PRIVATE -DG_LOG_DOMAIN=\"gnc.report.gui\")
target_compile_options(gncmod-report-gnome PRIVATE -Wno-deprecated-declarations)

target_include_directories(gncmod-report-gnome PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

if (APPLE)
  set_target_properties (gncmod-report-gnome PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
endif()

install(TARGETS gncmod-report-gnome
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

install(FILES ${report_gnome_HEADERS} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/gnucash)

# Scheme

set(report_gnome_SCHEME
    report-gnome.scm
)

set(GUILE_OUTPUT_DIR   gnucash/report)
set(GUILE_DEPENDS
  gncmod-report-gnome
  scm-engine
  scm-gettext
  scm-gnome-utils
  scm-gnc-module
  scm-scm
  scm-report-system
  scm-standard-reports
  scm-business-reports
  scm-utility-reports
  )

gnc_add_scheme_targets(scm-report-gnome
    "${report_gnome_SCHEME}"
    ${GUILE_OUTPUT_DIR}
    "${GUILE_DEPENDS}"
    FALSE
)

set_local_dist(report_gnome_DIST_local CMakeLists.txt report-gnome.i ${report_gnome_HEADERS}
    ${report_gnome_SOURCES} ${report_gnome_SCHEME})
set(report_gnome_DIST ${report_gnome_DIST_local} ${test_report_gnome_DIST} PARENT_SCOPE)
