
SET(APP_UTILS_TEST_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common # for config.h
  ${CMAKE_SOURCE_DIR}/common/test-core
  ${CMAKE_SOURCE_DIR}/libgnucash/app-utils
  ${CMAKE_SOURCE_DIR}/libgnucash/engine # for qof.h
  ${CMAKE_SOURCE_DIR}/libgnucash/engine/test-core
  ${GIO_INCLUDE_DIRS}
  ${GUILE_INCLUDE_DIRS}
)

SET(APP_UTILS_TEST_LIBS gncmod-app-utils gncmod-test-engine test-core ${GIO_LDFLAGS} ${GUILE_LDFLAGS})

SET(test_app_utils_SOURCES test-app-utils.c test-option-util.cpp test-gnc-ui-util.c)

MACRO(ADD_APP_UTILS_TEST _TARGET _SOURCE_FILES)
  GNC_ADD_TEST(${_TARGET} "${_SOURCE_FILES}" APP_UTILS_TEST_INCLUDE_DIRS APP_UTILS_TEST_LIBS)
ENDMACRO()

ADD_APP_UTILS_TEST(test-exp-parser test-exp-parser.c)
GNC_ADD_TEST_WITH_GUILE(test-link-module test-link-module APP_UTILS_TEST_INCLUDE_DIRS APP_UTILS_TEST_LIBS)
ADD_APP_UTILS_TEST(test-print-parse-amount test-print-parse-amount.cpp)
# This test not run in autotools build.
#GNC_ADD_TEST_WITH_GUILE(test-print-queries test-print-queries.cpp APP_UTILS_TEST_INCLUDE_DIRS APP_UTILS_TEST_LIBS)
GNC_ADD_TEST_WITH_GUILE(test-scm-query-string test-scm-query-string.cpp
  APP_UTILS_TEST_INCLUDE_DIRS APP_UTILS_TEST_LIBS
)
ADD_APP_UTILS_TEST(test-sx test-sx.cpp)

GNC_ADD_SCHEME_TEST(scm-test-load-module test-load-module.in)
# Doesn't work yet:
#ADD_APP_UTILS_TEST(test-app-utils "${test_app_utils_SOURCES}")

CONFIGURE_FILE(test-load-module.in test-load-module @ONLY)

SET_DIST_LIST(test_app_utils_DIST CMakeLists.txt Makefile.am test-exp-parser.c test-link-module.c test-load-module.in
        test-print-parse-amount.cpp test-print-queries.cpp test-scm-query-string.cpp test-sx.cpp ${test_app_utils_SOURCES})
