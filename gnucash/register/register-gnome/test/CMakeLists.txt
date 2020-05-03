
set(REGISTER_GNOME_TEST_INCLUDE_DIRS "")
set(REGISTER_GNOME_TEST_LIBS)

gnc_add_test(test-link-module-register-gnome test-link-module.c
  REGISTER_GNOME_TEST_INCLUDE_DIRS REGISTER_GNOME_TEST_LIBS
)

set_dist_list(test_register_gnome_DIST CMakeLists.txt test-link-module.c)