
set(REGISTER_CORE_TEST_INCLUDE_DIRS "")
set(REGISTER_CORE_TEST_LIBS)

gnc_add_test(test-link-module-register-core test-link-module.c
  REGISTER_CORE_TEST_INCLUDE_DIRS REGISTER_CORE_TEST_LIBS
)

set_dist_list(test_register_core_DIST CMakeLists.txt test-link-module.c)