
SET(APP_UTILS_TEST_INCLUDE_DIRS
  ${CMAKE_SOURCE_DIR}/src/app-utils
  ${CMAKE_SOURCE_DIR}/src/libqof/qof # for qof.h
  ${CMAKE_SOURCE_DIR}/src/test-core
  ${CMAKE_SOURCE_DIR}/src/engine/test-core
  ${CMAKE_BINARY_DIR}/src # for config.h
  ${GLIB2_INCLUDE_DIRS}
  ${GUILE_INCLUDE_DIRS}
)

SET(APP_UTILS_TEST_LIBS gncmod-app-utils gncmod-test-engine gnc-qof test-core ${GUILE_LDFLAGS})

MACRO(ADD_APP_UTILS_TEST _TARGET _SOURCE_FILES)
  GNC_ADD_TEST(${_TARGET} "${_SOURCE_FILES}" APP_UTILS_TEST_INCLUDE_DIRS APP_UTILS_TEST_LIBS)
ENDMACRO()

ADD_APP_UTILS_TEST(test-exp-parser test-exp-parser.c)
GNC_ADD_TEST_WITH_GUILE(test-link-module test-link-module APP_UTILS_TEST_INCLUDE_DIRS APP_UTILS_TEST_LIBS)
ADD_APP_UTILS_TEST(test-print-parse-amount test-print-parse-amount.c)
# This test not run in autotools build.
#GNC_ADD_TEST_WITH_GUILE(test-print-queries test-print-queries.c APP_UTILS_TEST_INCLUDE_DIRS APP_UTILS_TEST_LIBS)
GNC_ADD_TEST_WITH_GUILE(test-scm-query-string test-scm-query-string.c
  APP_UTILS_TEST_INCLUDE_DIRS APP_UTILS_TEST_LIBS
)
ADD_APP_UTILS_TEST(test-sx test-sx.c)

GNC_ADD_SCHEME_TEST(scm-test-load-module test-load-module.in)

CONFIGURE_FILE(test-load-module.in test-load-module @ONLY)

GNC_ADD_SCHEME_TEST(scm-test-date-utilities test-date-utilities.scm)

SET_DIST_LIST(test_app_utils_DIST CMakeLists.txt Makefile.am test-exp-parser.c test-link-module.c test-load-module.in
        test-print-parse-amount.c test-print-queries.c test-scm-query-string.c test-sx.c test-date-utilities.scm)
