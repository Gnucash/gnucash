
set(GNOME_UTILS_TEST_INCLUDE_DIRS
  ${CMAKE_SOURCE_DIR}/libgnucash/gnc-module
  ${GLIB2_INCLUDE_DIRS}
  ${GUILE_INCLUDE_DIRS}
)
set(GNOME_UTILS_TEST_LIBS gnc-module test-core)

gnc_add_test_with_guile(test-link-module-gnome-utils test-link-module.c
  GNOME_UTILS_TEST_INCLUDE_DIRS GNOME_UTILS_TEST_LIBS
)


set(GNOME_UTILS_GUI_TEST_INCLUDE_DIRS
  ${GNOME_UTILS_TEST_INCLUDE_DIRS}
  ${CMAKE_BINARY_DIR}/common
  ${CMAKE_SOURCE_DIR}/gnucash/gnome-utils
  ${CMAKE_SOURCE_DIR}/libgnucash/engine
#  ${GTK3_INCLUDE_DIRS}
)
set(GNOME_UTILS_GUI_TEST_LIBS
  ${GNOME_UTILS_TEST_LIBS}
  gncmod-gnome-utils
)
#This is a GUI test
#gnc_add_test(test-gnc-recurrence test-gnc-recurrence.c
#  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS
#  GNOME_UTILS_GUI_TEST_LIBS
#
set(GUILE_DEPENDS
  scm-gnc-module
  scm-gnome-utils
  test-core
  gncmod-test-engine
  gncmod-app-utils
  gnc-core-utils
  gncmod-engine
)

gnc_add_scheme_targets(scm-test-load-gnome-utils-module
  "test-load-gnome-utils-module.scm"
  "gnucash/reports"
  "${GUILE_DEPENDS}"
  FALSE
)

gnc_add_scheme_tests(test-load-gnome-utils-module.scm)


set_dist_list(test_gnome_utils_DIST CMakeLists.txt test-gnc-recurrence.c test-link-module.c test-load-gnome-utils-module.scm)
