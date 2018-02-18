ADD_SUBDIRECTORY(test)

SET(locale_reports_us_SOURCES
  gncmod-locale-reports-us.c
)

ADD_LIBRARY(gncmod-locale-reports-us ${locale_reports_us_SOURCES})

TARGET_LINK_LIBRARIES(gncmod-locale-reports-us gnc-module gncmod-app-utils gncmod-gnome-utils
                         ${GUILE_LDFLAGS} ${GLIB2_LDFLAGS})

TARGET_COMPILE_DEFINITIONS(gncmod-locale-reports-us PRIVATE -DG_LOG_DOMAIN=\"gnc.report.locale.us\")


IF (APPLE)
  SET_TARGET_PROPERTIES (gncmod-locale-reports-us PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
ENDIF()

INSTALL(TARGETS gncmod-locale-reports-us
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
# No headers to install

# Scheme

SET(gncmod_local_reports_us_SCHEME_1 taxtxf.scm taxtxf-de_DE.scm)


SET(gncmod_local_reports_us_SCHEME_2 us.scm de_DE.scm)


SET(GUILE_DEPENDS
  gncmod-html
  scm-core-utils
  scm-gettext
  scm-gnc-module
  scm-scm
  scm-test-core
  scm-tax-us
  scm-report-system
  scm-gnome-utils
  )

GNC_ADD_SCHEME_TARGETS(scm-report-locale-specific-1
  "${gncmod_local_reports_us_SCHEME_1}"
  gnucash/report
  "${GUILE_DEPENDS}"
  FALSE
)

GNC_ADD_SCHEME_TARGETS(scm-report-locale-specific-2
  "${gncmod_local_reports_us_SCHEME_2}"
  gnucash/report/locale-specific
  "scm-report-locale-specific-1;${GUILE_DEPENDS}"
  FALSE
)

SET_LOCAL_DIST(locale_specific_us_DIST_local CMakeLists.txt 
        ${locale_reports_us_SOURCES} ${gncmod_local_reports_us_SCHEME_1} ${gncmod_local_reports_us_SCHEME_2})

SET(locale_specific_us_DIST ${locale_specific_us_DIST_local} ${test_locale_specific_us_DIST} PARENT_SCOPE)
