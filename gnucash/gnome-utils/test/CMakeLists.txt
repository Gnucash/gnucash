
SET(GNOME_UTILS_TEST_INCLUDE_DIRS
  ${CMAKE_SOURCE_DIR}/libgnucash/gnc-module
  ${GLIB2_INCLUDE_DIRS}
  ${GUILE_INCLUDE_DIRS}
)
SET(GNOME_UTILS_TEST_LIBS gnc-module test-core)

GNC_ADD_TEST_WITH_GUILE(test-link-module-gnome-utils test-link-module.c
  GNOME_UTILS_TEST_INCLUDE_DIRS GNOME_UTILS_TEST_LIBS
)


SET(GNOME_UTILS_GUI_TEST_INCLUDE_DIRS
  ${GNOME_UTILS_TEST_INCLUDE_DIRS}
  ${CMAKE_BINARY_DIR}/common
  ${CMAKE_SOURCE_DIR}/gnucash/gnome-utils
  ${CMAKE_SOURCE_DIR}/libgnucash/engine
  ${GTK3_INCLUDE_DIRS}
)
SET(GNOME_UTILS_GUI_TEST_LIBS
  ${GNOME_UTILS_TEST_LIBS}
  gncmod-gnome-utils
)
#This is a GUI test
#GNC_ADD_TEST(test-gnc-recurrence test-gnc-recurrence.c
#  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS
#  GNOME_UTILS_GUI_TEST_LIBS
#
SET(GUILE_DEPENDS
  test-core
  gncmod-test-engine
  gncmod-app-utils
  gnc-core-utils
  gncmod-engine
)

GNC_ADD_SCHEME_TARGETS(scm-test-load-gnome-utils-module
  "test-load-gnome-utils-module.scm"
  "gnucash/reports"
  "${GUILE_DEPENDS}"
  FALSE
)

GNC_ADD_SCHEME_TESTS(test-load-gnome-utils-module.scm)


SET_DIST_LIST(test_gnome_utils_DIST CMakeLists.txt test-gnc-recurrence.c test-link-module.c test-load-gnome-utils-module.scm)
