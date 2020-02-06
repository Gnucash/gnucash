add_subdirectory(test)

set(stylesheets_SOURCES
  gnc-plugin-stylesheets.c
  gncmod-stylesheets.c
)

set(stylesheets_noinst_HEADERS
  gnc-plugin-stylesheets.h
)

add_library(gncmod-stylesheets MODULE ${stylesheets_SOURCES} ${stylesheets_noinst_HEADERS})

target_link_libraries(gncmod-stylesheets gncmod-report-gnome gncmod-report-system gncmod-gnome-utils gnc-module
    PkgConfig::GTK3 ${GUILE_LDFLAGS} ${GLIB2_LDFLAGS})

target_compile_definitions(gncmod-stylesheets PRIVATE -DG_LOG_DOMAIN=\"gnc.report.core\")

set(LIB_DIR ${CMAKE_INSTALL_LIBDIR}/gnucash)
if (WIN32)
  set(LIB_DIR ${CMAKE_INSTALL_BINDIR})
endif()


if (APPLE)
  set_target_properties (gncmod-stylesheets PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
endif()

install(TARGETS gncmod-stylesheets
  LIBRARY DESTINATION ${LIB_DIR}
  ARCHIVE DESTINATION ${LIB_DIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

# No headers to install

# Scheme

set(stylesheets_SCHEME_1
  stylesheet-css.scm
  stylesheet-plain.scm
  stylesheet-footer.scm
  stylesheet-head-or-tail.scm
)

set(stylesheets_SCHEME_2
  stylesheets.scm
)

set(GUILE_OUTPUT_DIR gnucash/report)
set(GUILE_DEPENDS
  scm-gnc-module
  scm-core-utils
  scm-gettext
  scm-scm
  gncmod-html
  scm-report-system
  )

gnc_add_scheme_targets(scm-report-stylesheets-1
  "${stylesheets_SCHEME_1}"
  ${GUILE_OUTPUT_DIR}
  "${GUILE_DEPENDS}"
  TRUE
)

gnc_add_scheme_targets(scm-report-stylesheets-2
  "${stylesheets_SCHEME_2}"
  ${GUILE_OUTPUT_DIR}
  "scm-report-stylesheets-1;${GUILE_DEPENDS}"
  TRUE
)

add_custom_target(scm-report-stylesheets ALL DEPENDS scm-report-stylesheets-1 scm-report-stylesheets-2)

set_local_dist(stylesheets_DIST_local CMakeLists.txt
        ${stylesheets_SOURCES} ${stylesheets_noinst_HEADERS}
        ${stylesheets_SCHEME_1} ${stylesheets_SCHEME_2})

set(stylesheets_DIST ${stylesheets_DIST_local} ${test_stylesheets_DIST} PARENT_SCOPE)
