
set(GNOME_UTILS_GUI_TEST_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common
  ${CMAKE_SOURCE_DIR}/gnucash/gnome-utils
  ${CMAKE_SOURCE_DIR}/libgnucash/engine
  ${GUILE_INCLUDE_DIRS}
)
set(GNOME_UTILS_GUI_TEST_LIBS
  test-core
  gnc-gnome-utils
)
#This is a GUI test
#gnc_add_test(test-gnc-recurrence test-gnc-recurrence.c
#  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS
#  GNOME_UTILS_GUI_TEST_LIBS
#
set(GUILE_DEPENDS
  scm-gnome-utils
  test-core
  gnc-test-engine
  gnc-app-utils
  gnc-core-utils
  gnc-engine
)

gnc_add_scheme_test_targets(scm-test-load-gnome-utils-module
    SOURCES "test-load-gnome-utils-module.scm"
    OUTPUT_DIR "tests"
    DEPENDS "${GUILE_DEPENDS}")

set(test_autoclear_SOURCES
  test-autoclear.cpp
)

set(test_autoclear_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common
  ${CMAKE_SOURCE_DIR}/libgnucash/engine
)

set(test_autoclear_LIBS
  gnc-engine
  gnc-gnome-utils
  gtest
)

gnc_add_test(test-autoclear "${test_autoclear_SOURCES}"
    test_autoclear_INCLUDE_DIRS
    test_autoclear_LIBS
)

gnc_add_scheme_tests(test-load-gnome-utils-module.scm)


set_dist_list(test_gnome_utils_DIST CMakeLists.txt test-gnc-recurrence.c test-load-gnome-utils-module.scm
  ${test_autoclear_SOURCES})
