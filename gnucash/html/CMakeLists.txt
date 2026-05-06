
set (html_HEADERS
  gnc-html-history.h
  gnc-html.h
  gnc-html-p.h
  gnc-html-factory.hpp
  gnc-html-extras.h
  gnc-html-webkit-p.hpp
  gnc-html-webkit.hpp
)

# Command to generate the swig-gnc-html.cpp wrapper file
set(SWIG_ARGS "-c++" "-procdoc" "sw-gnc-option-doc" "-procdocformat" "plain")
gnc_add_swig_guile_command (swig-gnc-html-cpp
    SWIG_GNC_HTML_CPP swig-gnc-html.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/gnc-html.i ${html_HEADERS}
)
unset(SWIG_ARGS)

set (html_SOURCES
  gnc-html.cpp
  gnc-html-history.cpp
  gnc-html-factory.cpp
)

if (WEBKIT1)
  list(APPEND html_HEADERS gnc-html-webkit1.hpp)
  list(APPEND html_SOURCES gnc-html-webkit1.cpp)
  set(html_EXTRA_DIST gnc-html-webkit2.hpp gnc-html-webkit2.cpp)
else ()
  list(APPEND html_HEADERS gnc-html-webkit2.hpp)
  list(APPEND html_SOURCES gnc-html-webkit2.cpp)
  set(html_EXTRA_DIST gnc-html-webkit1.hpp gnc-html-webkit1.cpp)
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
  ${SWIG_GNC_HTML_CPP}
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

add_dependencies(gnc-html swig-gnc-html-cpp)

if (COVERAGE)
  add_coverage_target(gnc-html)
endif()

install(TARGETS gnc-html
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

install(FILES ${html_HEADERS} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/gnucash)
