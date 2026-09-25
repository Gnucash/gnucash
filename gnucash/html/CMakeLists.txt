add_subdirectory(test)


set (html_HEADERS
  gnc-html-history.h
  gnc-html.h
  gnc-html-p.h
  gnc-html-factory.hpp
  gnc-html-extras.h
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

set (html_BACKEND_HEADERS
  gnc-html-webkit-p.hpp
  gnc-html-webkit2.hpp
  gnc-html-webview2.hpp
  gnc-html-wkwebview.hpp
)

set (html_BACKEND_SOURCES
  gnc-html-webkit2.cpp
  gnc-html-webview2.cpp
  gnc-html-wkwebview.mm
)

if (GNC_REPORT_WEBKITGTK6)
  list(APPEND html_HEADERS gnc-html-webkit2.hpp)
  list(APPEND html_SOURCES gnc-html-webkit2.cpp)
elseif (GNC_REPORT_WEBVIEW2)
  list(APPEND html_HEADERS gnc-html-webview2.hpp)
  list(APPEND html_SOURCES gnc-html-webview2.cpp)
elseif (GNC_REPORT_WKWEBVIEW)
  list(APPEND html_HEADERS gnc-html-wkwebview.hpp)
  list(APPEND html_SOURCES gnc-html-wkwebview.mm)
else ()
  message(FATAL_ERROR "No report backend selected")
endif()



set (gnc_html_SCHEME html.scm)

set(GUILE_OUTPUT_DIR   gnucash)
set(GUILE_DEPENDS      gnc-html gnucash-guile scm-core-utils)

gnc_add_scheme_targets(scm-gnc-html
    SOURCES "${gnc_html_SCHEME}"
    OUTPUT_DIR ${GUILE_OUTPUT_DIR}
    DEPENDS "${GUILE_DEPENDS}"
    MAKE_LINKS)

set (html_DIST_HEADERS ${html_HEADERS} ${html_BACKEND_HEADERS})
set (html_DIST_SOURCES ${html_SOURCES} ${html_BACKEND_SOURCES})
list(REMOVE_DUPLICATES html_DIST_HEADERS)
list(REMOVE_DUPLICATES html_DIST_SOURCES)

set_local_dist(html_DIST_local CMakeLists.txt ${html_DIST_HEADERS} ${html_DIST_SOURCES}
  gnc-html-webview2-clipboard.hpp gnc-html-webview2-coordinates.hpp
  gnc-html-webview2-visibility.hpp gnc-html-native-widget-lifecycle.hpp
  gnc-html-webview2-loader-state.hpp
  gnc-html.i ${html_EXTRA_DIST} html.scm)

add_library (gnc-html
  ${html_SOURCES}
  ${SWIG_GNC_HTML_CPP}
  ${html_HEADERS}
)

target_link_libraries(gnc-html
    PRIVATE
        gnc-engine
        gnc-gnome-utils
        PkgConfig::GTK4
        $<$<BOOL:${GNC_REPORT_WEBKITGTK6}>:PkgConfig::WEBKIT>
        ${WEBKIT_FRAMEWORK}
        ${GUILE_LDFLAGS})

target_compile_definitions(gnc-html PRIVATE -DG_LOG_DOMAIN=\"gnc.html\"
    $<$<BOOL:${GNC_REPORT_WEBVIEW2}>:GNC_REPORT_WEBVIEW2>
    $<$<BOOL:${GNC_REPORT_WEBVIEW2_FIXED_RUNTIME}>:GNC_REPORT_WEBVIEW2_FIXED_RUNTIME>
    $<$<BOOL:${GNC_REPORT_WKWEBVIEW}>:GNC_REPORT_WKWEBVIEW>)

if (GNC_REPORT_WEBVIEW2)
  target_link_libraries(gnc-html PRIVATE dcomp ole32)
  if (MINGW)
    # These warnings originate in the Windows SDK WebView2/DirectComposition headers.
    target_compile_options(gnc-html PRIVATE -Wno-unknown-pragmas -Wno-class-conversion)
  endif()
endif()

if (GNC_REPORT_WKWEBVIEW)
  target_compile_options(gnc-html PRIVATE ${OSX_EXTRA_COMPILE_FLAGS})
  target_link_libraries(gnc-html PRIVATE PkgConfig::GTK4MACOS ${OSX_EXTRA_LIBRARIES})
endif()


target_include_directories (gnc-html
PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
PRIVATE
    ${WEBVIEW2_INCLUDE_DIRS}
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

if (GNC_REPORT_WEBVIEW2)
  install(FILES "${WEBVIEW2_LOADER_DLL}" DESTINATION ${CMAKE_INSTALL_BINDIR})
  if (GNC_REPORT_WEBVIEW2_FIXED_RUNTIME)
    install(DIRECTORY "${WEBVIEW2_FIXED_RUNTIME_DIR}/"
      DESTINATION "${CMAKE_INSTALL_BINDIR}/WebView2Runtime")
  endif()
endif()

install(FILES ${html_HEADERS} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/gnucash)

set(html_DIST ${html_DIST_local} ${test_html_DIST} PARENT_SCOPE)
