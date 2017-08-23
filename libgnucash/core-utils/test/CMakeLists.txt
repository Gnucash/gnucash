

SET(CORE_UTILS_TEST_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common # for config.h
  ${CMAKE_SOURCE_DIR}/libgnucash/core-utils
  ${CMAKE_SOURCE_DIR}/common/test-core
  ${GLIB2_INCLUDE_DIRS}
)
SET(CORE_UTILS_TEST_LIBS gnc-core-utils test-core)

MACRO(ADD_CORE_UTILS_TEST _TARGET _SOURCE_FILES)
  GNC_ADD_TEST(${_TARGET} "${_SOURCE_FILES}" CORE_UTILS_TEST_INCLUDE_DIRS CORE_UTILS_TEST_LIBS)
ENDMACRO()

ADD_CORE_UTILS_TEST(test-gnc-glib-utils test-gnc-glib-utils.c)
ADD_CORE_UTILS_TEST(test-resolve-file-path test-resolve-file-path.c)
ADD_CORE_UTILS_TEST(test-userdata-dir test-userdata-dir.c)
ADD_CORE_UTILS_TEST(test-userdata-dir-invalid-home test-userdata-dir-invalid-home.c)

SET_DIST_LIST(test_core_utils_DIST CMakeLists.txt Makefile.am test-gnc-glib-utils.c
    test-resolve-file-path.c test-userdata-dir.c test-userdata-dir-invalid-home.c)
