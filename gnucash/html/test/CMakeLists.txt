set(MODULEPATH ${CMAKE_SOURCE_DIR}/gnucash/html)

set(test_gnc_html_webkit2_SOURCES
  gtest-gnc-html-webkit2.cpp
)

set(test_gnc_html_webkit2_INCLUDES
  ${CMAKE_BINARY_DIR}/common # for config.h
  ${MODULEPATH}
  ${CMAKE_SOURCE_DIR}/libgnucash/engine
  ${GUILE_INCLUDE_DIRS}
)

set(test_gnc_html_webkit2_LIBS
  gnc-html
  gnc-engine
  gnc-gnome-utils
  ctre
  PkgConfig::GTK3
  PkgConfig::WEBKIT
  ${GUILE_LDFLAGS}
  gtest
)

gnc_add_test(test-gnc-html-webkit2
  "${test_gnc_html_webkit2_SOURCES}"
  test_gnc_html_webkit2_INCLUDES
  test_gnc_html_webkit2_LIBS
)

set_dist_list(test_html_DIST CMakeLists.txt ${test_gnc_html_webkit2_SOURCES})
