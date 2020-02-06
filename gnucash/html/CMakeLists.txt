
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
    ${CMAKE_CURRENT_SOURCE_DIR}/gnc-html.i "${gncmod_html_HEADERS}"
)

set (html_SOURCES
  gncmod-html.c
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

set_dist_list(html_DIST CMakeLists.txt ${html_HEADERS} ${html_SOURCES} gnc-html.i ${html_EXTRA_DIST})

add_library (gncmod-html
  ${html_SOURCES}
  ${SWIG_GNC_HTML_C}
  ${html_HEADERS}
)

target_link_libraries(gncmod-html gncmod-engine gnc-module gncmod-gnome-utils
  PkgConfig::GTK3 PkgConfig::WEBKIT ${GUILE_LDFLAGS})

target_compile_definitions(gncmod-html PRIVATE -DG_LOG_DOMAIN=\"gnc.html\")


target_include_directories (gncmod-html
PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
)

if (APPLE)
  set_target_properties (gncmod-html PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
endif()

install(TARGETS gncmod-html
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

install(FILES ${html_HEADERS} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/gnucash)
