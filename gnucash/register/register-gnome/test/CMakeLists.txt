
SET(REGISTER_GNOME_TEST_INCLUDE_DIRS "")
SET(REGISTER_GNOME_TEST_LIBS)

GNC_ADD_TEST(test-link-module-register-gnome test-link-module.c
  REGISTER_GNOME_TEST_INCLUDE_DIRS REGISTER_GNOME_TEST_LIBS
)

SET_DIST_LIST(test_register_gnome_DIST CMakeLists.txt test-link-module.c)