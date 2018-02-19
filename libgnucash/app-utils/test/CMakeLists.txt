
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

GNC_ADD_TEST_WITH_GUILE(test-exp-parser test-exp-parser.c
  APP_UTILS_TEST_INCLUDE_DIRS APP_UTILS_TEST_LIBS
 )
GNC_ADD_TEST_WITH_GUILE(test-link-module-app-utils test-link-module APP_UTILS_TEST_INCLUDE_DIRS APP_UTILS_TEST_LIBS)
ADD_APP_UTILS_TEST(test-print-parse-amount test-print-parse-amount.cpp)
# FIXME Why is this test not run ?
#GNC_ADD_TEST_WITH_GUILE(test-print-queries test-print-queries.cpp APP_UTILS_TEST_INCLUDE_DIRS APP_UTILS_TEST_LIBS)
GNC_ADD_TEST_WITH_GUILE(test-scm-query-string test-scm-query-string.cpp
  APP_UTILS_TEST_INCLUDE_DIRS APP_UTILS_TEST_LIBS
)
ADD_APP_UTILS_TEST(test-sx test-sx.cpp)

SET(GUILE_DEPENDS
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
  test-date-utilities.scm
)

GNC_ADD_SCHEME_TARGETS(scm-test-load-app-utils-module
  "test-load-app-utils-module.scm"
  "gnucash/reports"
  "${GUILE_DEPENDS}"
  FALSE
)

GNC_ADD_SCHEME_TARGETS(scm-test-c-interface
  "test-c-interface.scm"
  ""
  "${GUILE_DEPENDS}"
  FALSE
)

GNC_ADD_SCHEME_TESTS("${test_app_utils_scheme_SOURCES}")
# Doesn't work yet:
GNC_ADD_TEST_WITH_GUILE(test-app-utils "${test_app_utils_SOURCES}" APP_UTILS_TEST_INCLUDE_DIRS APP_UTILS_TEST_LIBS)

SET_DIST_LIST(test_app_utils_DIST
  CMakeLists.txt
  
  test-exp-parser.c
  test-link-module.c
  test-print-parse-amount.cpp
  test-print-queries.cpp
  test-scm-query-string.cpp
  test-sx.cpp
  test-c-interface.scm
  ${test_app_utils_scheme_SOURCES}
  ${test_app_utils_SOURCES}
)
