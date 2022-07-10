set(MODULEPATH ${CMAKE_SOURCE_DIR}/libgnucash/app-utils)

set(APP_UTILS_TEST_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common # for config.h
  ${CMAKE_SOURCE_DIR}/common/test-core
  ${CMAKE_SOURCE_DIR}/libgnucash/app-utils
  ${CMAKE_SOURCE_DIR}/libgnucash/engine # for qof.h
  ${CMAKE_SOURCE_DIR}/libgnucash/engine/test-core
  ${GIO_INCLUDE_DIRS}
  ${GUILE_INCLUDE_DIRS}
)

set(APP_UTILS_TEST_LIBS
        gnc-app-utils
        gnc-expressions
        gnc-test-engine
        test-core
        ${GIO_LDFLAGS}
        ${GUILE_LDFLAGS})

macro(add_app_utils_test _TARGET _SOURCE_FILES)
  gnc_add_test(${_TARGET} "${_SOURCE_FILES}" APP_UTILS_TEST_INCLUDE_DIRS APP_UTILS_TEST_LIBS)
endmacro()

gnc_add_test_with_guile(test-exp-parser test-exp-parser.c
  APP_UTILS_TEST_INCLUDE_DIRS APP_UTILS_TEST_LIBS
 )
add_app_utils_test(test-print-parse-amount test-print-parse-amount.cpp)
# FIXME Why is this test not run ?
#gnc_add_test_with_guile(test-print-queries test-print-queries.cpp APP_UTILS_TEST_INCLUDE_DIRS APP_UTILS_TEST_LIBS)
gnc_add_test_with_guile(test-scm-query-string test-scm-query-string.cpp
  APP_UTILS_TEST_INCLUDE_DIRS APP_UTILS_TEST_LIBS
)
add_app_utils_test(test-sx test-sx.cpp)

set(gtest_gnc_option_SOURCES
  gtest-gnc-option.cpp
  gtest-gnc-optiondb.cpp)

set(gtest_gnc_option_INCLUDES
  ${MODULEPATH}
  ${CMAKE_SOURCE_DIR}/libgnucash/engine
  ${CMAKE_BINARY_DIR}/common # for config.h
  ${GLIB2_INCLUDE_DIRS}
  ${GUILE_INCLUDE_DIRS})

set(gtest_gnc_option_LIBS
  gnc-app-utils
  gnc-engine
  ${GLIB2_LDFLAGS}
  ${GUILE_LDFLAGS}
  gtest)

gnc_add_test(test-gnc-option "${gtest_gnc_option_SOURCES}" gtest_gnc_option_INCLUDES gtest_gnc_option_LIBS)

set(GUILE_DEPENDS
  scm-test-engine
  scm-app-utils
  scm-core-utils
  scm-engine
  scm-core-utils
  gncmod-backend-xml
)

set(test_app_utils_scheme_SOURCES
  test-c-interface.scm
  test-load-app-utils-module.scm
)

set (test_app_utils_scheme_SRFI64_SOURCES
  test-date-utilities.scm
  test-options.scm
)

gnc_add_scheme_test_targets(scm-test-load-app-utils-module
    SOURCES "test-load-app-utils-module.scm"
    OUTPUT_DIR "tests"
    DEPENDS "${GUILE_DEPENDS}")

gnc_add_scheme_test_targets(scm-test-c-interface
    SOURCES "test-c-interface.scm"
    OUTPUT_DIR "tests"
    DEPENDS "${GUILE_DEPENDS}")

gnc_add_scheme_tests("${test_app_utils_scheme_SOURCES}")

if (HAVE_SRFI64)
  gnc_add_scheme_test_targets(scm-test-app-utils-srfi64
    SOURCES "${test_app_utils_scheme_SRFI64_SOURCES}"
    OUTPUT_DIR "tests"
    DEPENDS "${GUILE_DEPENDS};scm-srfi64-extras")

  gnc_add_scheme_test_targets(scm-test-gnc-optiondb
    SOURCES "test-gnc-optiondb.scm" "test-gnc-option-scheme-output.scm"
    OUTPUT_DIR "tests"
    DEPENDS "swig-apputils-guile-cpp;scm-srfi64-extras")
  gnc_add_scheme_tests("test-gnc-optiondb.scm")
  gnc_add_scheme_tests("test-gnc-option-scheme-output.scm")
  gnc_add_scheme_tests("${test_app_utils_scheme_SRFI64_SOURCES}")
endif()

set_dist_list(test_app_utils_DIST
  CMakeLists.txt
  test-exp-parser.c
  test-print-parse-amount.cpp
  test-print-queries.cpp
  test-scm-query-string.cpp
  test-sx.cpp
  gtest-gnc-option.cpp
  gtest-gnc-optiondb.cpp
  test-c-interface.scm
  test-date-utilities.scm
  test-options.scm
  test-gnc-optiondb.scm
  test-gnc-option-scheme-output.scm
  ${test_app_utils_scheme_SOURCES}
  ${test_app_utils_SOURCES}
)
