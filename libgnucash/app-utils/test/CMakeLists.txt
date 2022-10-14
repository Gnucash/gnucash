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
add_dependencies(test-exp-parser scm-expressions)
add_app_utils_test(test-print-parse-amount test-print-parse-amount.cpp)
gnc_add_test_with_guile(test-sx test-sx.cpp
  APP_UTILS_TEST_INCLUDE_DIRS APP_UTILS_TEST_LIBS
)

set(test_gnc_quotes_SOURCES
        gtest-gnc-quotes.cpp
        )

set(test_gnc_quotes_INCLUDES
        ${CMAKE_BINARY_DIR}/common # for config.h
        ${MODULEPATH}
        )

set(test_gnc_quotes_LIBS
        gnc-engine
        gtest
        ${Boost_FILESYSTEM_LIBRARY}
        ${Boost_LOCALE_LIBRARY}
        ${Boost_PROPERTY_TREE_LIBRARY}
        ${Boost_SYSTEM_LIBRARY}
        )
gnc_add_test(test-gnc-quotes "${test_gnc_quotes_SOURCES}" test_gnc_quotes_INCLUDES test_gnc_quotes_LIBS)

set(GUILE_DEPENDS
  scm-test-engine
  scm-app-utils
  scm-core-utils
  scm-engine
  scm-core-utils
  gncmod-backend-xml
)

set_dist_list(test_app_utils_DIST
  CMakeLists.txt
  gtest-gnc-quotes.cpp
  test-exp-parser.c
  test-print-parse-amount.cpp
  test-sx.cpp
  ${test_app_utils_scheme_SOURCES}
  ${test_app_utils_SOURCES}
)
