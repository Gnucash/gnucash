ADD_SUBDIRECTORY(test)

SET(locale_reports_us_SOURCES
  gncmod-locale-reports-us.c
)

ADD_LIBRARY(gncmod-locale-reports-us ${locale_reports_us_SOURCES})

TARGET_LINK_LIBRARIES(gncmod-locale-reports-us gnc-module gncmod-app-utils gncmod-gnome-utils
                         ${GUILE_LDFLAGS} ${GLIB2_LDFLAGS})

TARGET_COMPILE_DEFINITIONS(gncmod-locale-reports-us PRIVATE -DG_LOG_DOMAIN=\"gnc.report.locale.us\")


INSTALL(TARGETS gncmod-locale-reports-us
  LIBRARY DESTINATION lib/gnucash
  ARCHIVE DESTINATION lib/gnucash
  RUNTIME DESTINATION bin)
# No headers to install

# Scheme

SET(gncmod_local_reports_us_SCHEME_1 taxtxf.scm taxtxf-de_DE.scm)


SET(gncmod_local_reports_us_SCHEME_2 us.scm de_DE.scm)


SET(GUILE_MODULES src/app-utils src/engine src/gnome-utils src/html src/report/report-system src/tax/us)
SET(GUILE_LOAD_DIRS  src/app-utils src/core-utils src/engine src/gnc-module src/gnome-utils src/report/report-system
                     src/scm src/tax/us)
SET(GUILE_LIBRARY_DIRS src/engine src/libqof/qof src/app-utils src/core-utils src/gnc-module src/gnome-utils
                       src/html src/report/report-system)
SET(GUILE_DEPENDS   gncmod-html scm-tax-us-1 scm-tax-us-2 scm-report-system-3)

GNC_ADD_SCHEME_TARGETS(scm-report-locale-specific-1
  "${gncmod_local_reports_us_SCHEME_1}"
  gnucash/report
  "${GUILE_MODULES}"
  "${GUILE_LOAD_DIRS}"
  "${GUILE_LIBRARY_DIRS}"
  "${GUILE_DEPENDS}"
  FALSE
)

GNC_ADD_SCHEME_TARGETS(scm-report-locale-specific-2
  "${gncmod_local_reports_us_SCHEME_2}"
  gnucash/report/locale-specific
  "${GUILE_MODULES}"
  "${GUILE_LOAD_DIRS}"
  "${GUILE_LIBRARY_DIRS}"
  "scm-report-locale-specific-1;${GUILE_DEPENDS}"
  FALSE
)

SET_LOCAL_DIST(locale_specific_us_DIST_local CMakeLists.txt Makefile.am
        ${locale_reports_us_SOURCES} ${gncmod_local_reports_us_SCHEME_1} ${gncmod_local_reports_us_SCHEME_2})

SET(locale_specific_us_DIST ${locale_specific_us_DIST_local} ${test_locale_specific_us_DIST} PARENT_SCOPE)