
set (test-gnc-html-support_SOURCES
  test-gnc-html-support.cpp
  ${CMAKE_SOURCE_DIR}/gnucash/html/gnc-html-support.cpp
)

set (test-gnc-html-support_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common
  ${CMAKE_SOURCE_DIR}/gnucash/html
)

# gnc-html-support.cpp is deliberately free of GnuCash, GTK and browser
# dependencies, so it is compiled straight into the test rather than
# linked from libgnc-html: the tests then run on every platform,
# including the ones whose rendering backend can't be built. glib is
# here only for the g_strdup_printf() the error-page test compares
# against.
set (test-gnc-html-support_LIBS
  PkgConfig::GLIB2
  gtest
)

gnc_add_test (test-gnc-html-support
  "${test-gnc-html-support_SOURCES}"
  test-gnc-html-support_INCLUDE_DIRS
  test-gnc-html-support_LIBS
)

set_dist_list (test_html_DIST
  CMakeLists.txt
  test-gnc-html-support.cpp
)
