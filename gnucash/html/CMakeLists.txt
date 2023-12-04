
set (html_HEADERS
  gnc-html-history.h
  gnc-html.h
  gnc-html-p.h
  gnc-html-factory.h
  gnc-html-extras.h
  gnc-html-webkit-p.h
  gnc-html-webkit.h
)

# Command to generate the swig-gnc-html.c wrapper file
gnc_add_swig_guile_command (swig-gnc-html-c
    SWIG_GNC_HTML_C swig-gnc-html.c
    ${CMAKE_CURRENT_SOURCE_DIR}/gnc-html.i "" "${html_HEADERS}"
)

set (html_SOURCES
  gnc-html.c
  gnc-html-history.c
  gnc-html-factory.c
)

if (WEBKIT1)
  list(APPEND html_HEADERS gnc-html-webkit1.h)
  list(APPEND html_SOURCES gnc-html-webkit1.c)
  set(html_EXTRA_DIST gnc-html-webkit2.h gnc-html-webkit2.c)
else ()
  list(APPEND html_HEADERS gnc-html-webkit2.h)
  list(APPEND html_SOURCES gnc-html-webkit2.c)
  set(html_EXTRA_DIST gnc-html-webkit1.h gnc-html-webkit1.c)
endif()



set (gnc_html_SCHEME html.scm)

set(GUILE_OUTPUT_DIR   gnucash)
set(GUILE_DEPENDS      gnc-html gnucash-guile scm-core-utils)

gnc_add_scheme_targets(scm-gnc-html
    SOURCES "${gnc_html_SCHEME}"
    OUTPUT_DIR ${GUILE_OUTPUT_DIR}
    DEPENDS "${GUILE_DEPENDS}"
    MAKE_LINKS)

set_dist_list(html_DIST CMakeLists.txt ${html_HEADERS} ${html_SOURCES} gnc-html.i ${html_EXTRA_DIST} html.scm)

add_library (gnc-html
  ${html_SOURCES}
  ${SWIG_GNC_HTML_C}
  ${html_HEADERS}
)

target_link_libraries(gnc-html
    PRIVATE
        gnc-engine
        gnc-gnome-utils
        PkgConfig::GTK3
        PkgConfig::WEBKIT
        ${GUILE_LDFLAGS})

target_compile_definitions(gnc-html PRIVATE -DG_LOG_DOMAIN=\"gnc.html\")


target_include_directories (gnc-html
PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
)

if (APPLE)
  set_target_properties (gnc-html PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
endif()

add_dependencies(gnc-html swig-gnc-html-c)

if (COVERAGE)
  add_coverage_target(gnc-html)
endif()

install(TARGETS gnc-html
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

install(FILES ${html_HEADERS} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/gnucash)
