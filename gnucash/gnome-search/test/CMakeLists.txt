set (test_gnome_search_SOURCES
  gtest-search-account.cpp
)

set (test_gnome_search_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common
  ${CMAKE_SOURCE_DIR}/common/test-core
  ${CMAKE_SOURCE_DIR}/libgnucash/engine
  ${CMAKE_SOURCE_DIR}/gnucash/gnome-utils
  ${CMAKE_SOURCE_DIR}/gnucash/gnome-search
  ${GLIB2_INCLUDE_DIRS}
)

set (test_gnome_search_LIBS
  gnc-gnome-search
  gnc-gnome-utils
  gnc-engine
  gtest
)

gnc_add_test (test-gnome-search
  "${test_gnome_search_SOURCES}"
  test_gnome_search_INCLUDE_DIRS
  test_gnome_search_LIBS
)

set_dist_list (test_gnome_search_DIST
  CMakeLists.txt
  ${test_gnome_search_SOURCES}
)
