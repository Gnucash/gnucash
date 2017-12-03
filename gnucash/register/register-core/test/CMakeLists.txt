
SET(REGISTER_CORE_TEST_INCLUDE_DIRS "")
SET(REGISTER_CORE_TEST_LIBS)

GNC_ADD_TEST(test-link-module-register-core test-link-module.c
  REGISTER_CORE_TEST_INCLUDE_DIRS REGISTER_CORE_TEST_LIBS
)

SET_DIST_LIST(test_register_core_DIST CMakeLists.txt test-link-module.c)