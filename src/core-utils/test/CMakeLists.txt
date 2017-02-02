

SET(CORE_UTILS_TEST_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/src # for config.h
  ${CMAKE_SOURCE_DIR}/src/core-utils
  ${CMAKE_SOURCE_DIR}/src/test-core
  ${CMAKE_SOURCE_DIR}/src/libqof/qof # for qof.h
  ${GLIB2_INCLUDE_DIRS}
)
SET(CORE_UTILS_TEST_LIBS gnc-core-utils gnc-qof test-core)

MACRO(ADD_CORE_UTILS_TEST _TARGET _SOURCE_FILES)
  GNC_ADD_TEST(${_TARGET} "${_SOURCE_FILES}" CORE_UTILS_TEST_INCLUDE_DIRS CORE_UTILS_TEST_LIBS)
ENDMACRO()

ADD_CORE_UTILS_TEST(test-gnc-glib-utils test-gnc-glib-utils.c)
ADD_CORE_UTILS_TEST(test-gnc-uri-utils test-gnc-uri-utils.c)
ADD_CORE_UTILS_TEST(test-resolve-file-path test-resolve-file-path.c)

SET_DIST_LIST(test_core_utils_DIST CMakeLists.txt Makefile.am test-gnc-glib-utils.c test-gnc-uri-utils.c test-resolve-file-path.c)