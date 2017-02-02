
SET (html_HEADERS
  gnc-html-history.h
  gnc-html.h
  gnc-html-p.h
  gnc-html-factory.h
  gnc-html-webkit.h
  gnc-html-extras.h
  gnc-html-webkit-p.h
)

IF (BUILDING_FROM_VCS)
  SET (SWIG_GNC_HTML_C ${CMAKE_CURRENT_BINARY_DIR}/swig-gnc-html.c)
  GNC_ADD_SWIG_COMMAND (swig-gnc-html-c ${SWIG_GNC_HTML_C} ${CMAKE_CURRENT_SOURCE_DIR}/gnc-html.i "${gncmod_html_HEADERS}")
ELSE()
  SET (SWIG_GNC_HTML_C swig-gnc-html.c)
ENDIF()

SET (html_SOURCES
  gncmod-html.c
  gnc-html.c
  gnc-html-history.c
  gnc-html-factory.c
  gnc-html-webkit.c
)

SET_DIST_LIST(html_DIST CMakeLists.txt Makefile.am ${html_HEADERS} ${html_SOURCES} gnc-html.i)

ADD_LIBRARY (gncmod-html
  ${html_SOURCES}
  ${SWIG_GNC_HTML_C}
  ${html_HEADERS}
)

TARGET_LINK_LIBRARIES(gncmod-html gnc-qof gnc-module gncmod-gnome-utils ${WEBKIT_LDFLAGS} ${GUILE_LDFLAGS})

TARGET_COMPILE_DEFINITIONS(gncmod-html PRIVATE -DG_LOG_DOMAIN=\"gnc.html\")


TARGET_INCLUDE_DIRECTORIES (gncmod-html
    PUBLIC ${WEBKIT_INCLUDE_DIRS} ${CMAKE_CURRENT_SOURCE_DIR}
)

INSTALL(TARGETS gncmod-html
  LIBRARY DESTINATION lib/gnucash
  ARCHIVE DESTINATION lib/gnucash
  RUNTIME DESTINATION bin)

INSTALL(FILES ${html_HEADERS} DESTINATION include/gnucash)
