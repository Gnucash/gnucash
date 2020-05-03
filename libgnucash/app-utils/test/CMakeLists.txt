
set(APP_UTILS_TEST_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common # for config.h
  ${CMAKE_SOURCE_DIR}/common/test-core
  ${CMAKE_SOURCE_DIR}/libgnucash/app-utils
  ${CMAKE_SOURCE_DIR}/libgnucash/engine # for qof.h
  ${CMAKE_SOURCE_DIR}/libgnucash/engine/test-core
  ${GIO_INCLUDE_DIRS}
  ${GUILE_INCLUDE_DIRS}
)

set(APP_UTILS_TEST_LIBS gncmod-app-utils gncmod-test-engine test-core ${GIO_LDFLAGS} ${GUILE_LDFLAGS})

set(test_app_utils_SOURCES test-app-utils.c test-option-util.cpp test-gnc-ui-util.c)

macro(add_app_utils_test _TARGET _SOURCE_FILES)
  gnc_add_test(${_TARGET} "${_SOURCE_FILES}" APP_UTILS_TEST_INCLUDE_DIRS APP_UTILS_TEST_LIBS)
endmacro()

gnc_add_test_with_guile(test-exp-parser test-exp-parser.c
  APP_UTILS_TEST_INCLUDE_DIRS APP_UTILS_TEST_LIBS
 )
gnc_add_test_with_guile(test-link-module-app-utils test-link-module APP_UTILS_TEST_INCLUDE_DIRS APP_UTILS_TEST_LIBS)
add_app_utils_test(test-print-parse-amount test-print-parse-amount.cpp)
# FIXME Why is this test not run ?
#gnc_add_test_with_guile(test-print-queries test-print-queries.cpp APP_UTILS_TEST_INCLUDE_DIRS APP_UTILS_TEST_LIBS)
gnc_add_test_with_guile(test-scm-query-string test-scm-query-string.cpp
  APP_UTILS_TEST_INCLUDE_DIRS APP_UTILS_TEST_LIBS
)
add_app_utils_test(test-sx test-sx.cpp)

set(GUILE_DEPENDS
  scm-test-engine
  scm-app-utils
  scm-core-utils
  scm-gnc-module
  scm-engine
  scm-scm
  scm-gettext
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

gnc_add_scheme_targets(scm-test-load-app-utils-module
  "test-load-app-utils-module.scm"
  "gnucash/reports"
  "${GUILE_DEPENDS}"
  FALSE
)

gnc_add_scheme_targets(scm-test-c-interface
  "test-c-interface.scm"
  ""
  "${GUILE_DEPENDS}"
  FALSE
)

gnc_add_scheme_tests("${test_app_utils_scheme_SOURCES}")

if (HAVE_SRFI64)
  gnc_add_scheme_tests("${test_app_utils_scheme_SRFI64_SOURCES}")
endif()

# Doesn't work yet:
gnc_add_test_with_guile(test-app-utils "${test_app_utils_SOURCES}" APP_UTILS_TEST_INCLUDE_DIRS APP_UTILS_TEST_LIBS)

set_dist_list(test_app_utils_DIST
  CMakeLists.txt
  
  test-exp-parser.c
  test-link-module.c
  test-print-parse-amount.cpp
  test-print-queries.cpp
  test-scm-query-string.cpp
  test-sx.cpp
  test-c-interface.scm
  test-date-utilities.scm
  test-options.scm
  ${test_app_utils_scheme_SOURCES}
  ${test_app_utils_SOURCES}
)
