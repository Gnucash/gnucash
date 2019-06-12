add_subdirectory(test)

set(locale_specific_SOURCES
  gncmod-locale-specific.c
)

add_library(gncmod-locale-specific ${locale_specific_SOURCES})

target_link_libraries(gncmod-locale-specific gnc-module gncmod-app-utils gncmod-gnome-utils
                         ${GUILE_LDFLAGS} ${GLIB2_LDFLAGS})

target_compile_definitions(gncmod-locale-specific PRIVATE -DG_LOG_DOMAIN=\"gnc.report.locale\")


if (APPLE)
  set_target_properties (gncmod-locale-specific PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
endif()

install(TARGETS gncmod-locale-specific
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
# No headers to install

# Scheme

set(gncmod_locale_reports_SCHEME taxtxf.scm taxtxf-de_DE.scm)

set(GUILE_DEPENDS
  gncmod-html
  scm-core-utils
  scm-gettext
  scm-gnc-module
  scm-scm
  scm-test-core
  scm-tax-us
  scm-report
  scm-gnome-utils
  )

gnc_add_scheme_targets(scm-reports-locale-specific-1
  "${gncmod_locale_reports_SCHEME}"
  gnucash/report/reports/locale-specific
  "${GUILE_DEPENDS}"
  FALSE
)

add_custom_target(scm-reports-locale-specific ALL DEPENDS scm-reports-locale-specific-1)

set_local_dist(reports_locale_specific_DIST_local CMakeLists.txt
        ${locale_specific_SOURCES} ${gncmod_locale_reports_SCHEME})

set(reports_locale_specific_DIST ${reports_locale_specific_DIST_local} ${test_locale_specific_DIST} PARENT_SCOPE)
