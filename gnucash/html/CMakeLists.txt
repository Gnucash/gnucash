
set (html_HEADERS
  gnc-html-history.h
  gnc-html.h
  gnc-html-p.h
  gnc-html-factory.hpp
  gnc-html-extras.h
  gnc-html-webkit-p.hpp
  gnc-html-webview2-p.hpp
  gnc-html-wkwebview-p.hpp
  gnc-html-webkit.hpp
  gnc-html-support.hpp
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
  gnc-html-support.cpp
)

# Exactly one rendering backend is compiled; the other two still have to
# be shipped in the tarball, so start with all three and take out
# whichever one is being built.
set (html_EXTRA_DIST
  gnc-html-webkit2.hpp gnc-html-webkit2.cpp
  gnc-html-webview2.hpp gnc-html-webview2.cpp
  gnc-html-wkwebview.hpp gnc-html-wkwebview.mm
)

if (WEBVIEW2)
  list(APPEND html_HEADERS gnc-html-webview2.hpp)
  list(APPEND html_SOURCES gnc-html-webview2.cpp)
  list(REMOVE_ITEM html_EXTRA_DIST gnc-html-webview2.hpp gnc-html-webview2.cpp)
elseif (WKWEBVIEW)
  list(APPEND html_HEADERS gnc-html-wkwebview.hpp)
  list(APPEND html_SOURCES gnc-html-wkwebview.mm)
  list(REMOVE_ITEM html_EXTRA_DIST gnc-html-wkwebview.hpp gnc-html-wkwebview.mm)
else ()
  list(APPEND html_HEADERS gnc-html-webkit2.hpp)
  list(APPEND html_SOURCES gnc-html-webkit2.cpp)
  list(REMOVE_ITEM html_EXTRA_DIST gnc-html-webkit2.hpp gnc-html-webkit2.cpp)
endif()

add_subdirectory(test)



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
        ${GUILE_LDFLAGS})

if (WEBVIEW2)
# The MSYS2 webview2-loader package doesn't include a link (.dll.a)
# library so we have to dlopen the library at runtime. We just need to
# include the header here.
  target_include_directories(gnc-html PRIVATE ${WEBVIEW2_INCLUDE_DIRS})
elseif (WKWEBVIEW)
# Only gnc-html-wkwebview.mm is Objective-C++, so ask for that dialect
# on that one source rather than forcing it on the whole target the way
# OSX_EXTRA_COMPILE_FLAGS does.
  set_source_files_properties(gnc-html-wkwebview.mm PROPERTIES
    COMPILE_OPTIONS "-xobjective-c++")
  target_link_libraries(gnc-html PRIVATE ${WEBKIT_FRAMEWORK} ${COCOA_LIBRARY} objc)
else()
  target_link_libraries(gnc-html PRIVATE PkgConfig::WEBKIT)
endif()

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
