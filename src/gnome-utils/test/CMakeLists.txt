
SET(GNOME_UTILS_TEST_INCLUDE_DIRS
  ${CMAKE_SOURCE_DIR}/src/gnc-module
  ${GLIB2_INCLUDE_DIRS}
  ${GUILE_INCLUDE_DIRS}
)
SET(GNOME_UTILS_TEST_LIBS gnc-module test-core)

GNC_ADD_TEST_WITH_GUILE(test-link-module-gnome-utils test-link-module.c
  GNOME_UTILS_TEST_INCLUDE_DIRS GNOME_UTILS_TEST_LIBS
)


SET(GNOME_UTILS_GUI_TEST_INCLUDE_DIRS
  ${GNOME_UTILS_TEST_INCLUDE_DIRS}
  ${CMAKE_BINARY_DIR}/src
  ${CMAKE_SOURCE_DIR}/src/gnome-utils
  ${CMAKE_SOURCE_DIR}/src/engine
  ${CMAKE_SOURCE_DIR}/src/libqof/qof # for qof.h
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

GNC_ADD_SCHEME_TEST(test-load-module-gnome-utils test-load-module.in)
