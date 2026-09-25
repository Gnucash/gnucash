set(HTML_TEST_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common
  ${CMAKE_SOURCE_DIR}/gnucash/html
)

set(HTML_TEST_LIBS
  gnc-html
  PkgConfig::GTK4
)

set(test_gnc_html_url_policy_SOURCES
  test-gnc-html-url-policy.cpp
)

set(test_gnc_html_report_document_SOURCES
  test-gnc-html-report-document.cpp
)

set(test_gnc_html_webview2_clipboard_SOURCES
  test-gnc-html-webview2-clipboard.cpp
)

set(test_gnc_html_webview2_coordinates_SOURCES
  test-gnc-html-webview2-coordinates.cpp
)

set(test_gnc_html_webview2_visibility_SOURCES
  test-gnc-html-webview2-visibility.cpp
)

set(test_gnc_html_native_widget_lifecycle_SOURCES
  test-gnc-html-native-widget-lifecycle.cpp
)

set(test_gnc_html_webview2_loader_state_SOURCES
  test-gnc-html-webview2-loader-state.cpp
)

gnc_add_test(test-gnc-html-url-policy
  "${test_gnc_html_url_policy_SOURCES}"
  HTML_TEST_INCLUDE_DIRS
  HTML_TEST_LIBS
)

gnc_add_test(test-gnc-html-report-document
  "${test_gnc_html_report_document_SOURCES}"
  HTML_TEST_INCLUDE_DIRS
  HTML_TEST_LIBS
)

gnc_add_test(test-gnc-html-webview2-clipboard
  "${test_gnc_html_webview2_clipboard_SOURCES}"
  HTML_TEST_INCLUDE_DIRS
  HTML_TEST_LIBS
)

gnc_add_test(test-gnc-html-webview2-coordinates
  "${test_gnc_html_webview2_coordinates_SOURCES}"
  HTML_TEST_INCLUDE_DIRS
  HTML_TEST_LIBS
)

gnc_add_test(test-gnc-html-webview2-visibility
  "${test_gnc_html_webview2_visibility_SOURCES}"
  HTML_TEST_INCLUDE_DIRS
  HTML_TEST_LIBS
)

gnc_add_test(test-gnc-html-native-widget-lifecycle
  "${test_gnc_html_native_widget_lifecycle_SOURCES}"
  HTML_TEST_INCLUDE_DIRS
  HTML_TEST_LIBS
)

gnc_add_test(test-gnc-html-webview2-loader-state
  "${test_gnc_html_webview2_loader_state_SOURCES}"
  HTML_TEST_INCLUDE_DIRS
  HTML_TEST_LIBS
)

add_test(NAME test-gnc-html-dist-manifest
  COMMAND ${CMAKE_COMMAND}
    -D DIST_MANIFEST=${CMAKE_BINARY_DIR}/dist_manifest.txt
    -P ${CMAKE_CURRENT_SOURCE_DIR}/test-gnc-html-dist-manifest.cmake)

set_dist_list(test_html_DIST
  CMakeLists.txt
  test-gnc-html-dist-manifest.cmake
  ${test_gnc_html_url_policy_SOURCES}
  ${test_gnc_html_report_document_SOURCES}
  ${test_gnc_html_webview2_clipboard_SOURCES}
  ${test_gnc_html_webview2_coordinates_SOURCES}
  ${test_gnc_html_webview2_visibility_SOURCES}
  ${test_gnc_html_native_widget_lifecycle_SOURCES}
  ${test_gnc_html_webview2_loader_state_SOURCES}
)
