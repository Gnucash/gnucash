

set(GENERIC_IMPORT_TEST_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common # for config.h
  ${CMAKE_SOURCE_DIR}/gnucash/import-export
  ${CMAKE_SOURCE_DIR}/common/test-core
  ${CMAKE_SOURCE_DIR}/libgnucash/engine
  ${CMAKE_SOURCE_DIR}/libgnucash/engine/test-core
  ${GUILE_INCLUDE_DIRS}
)

set(GENERIC_IMPORT_TEST_LIBS gnc-generic-import gnc-test-engine test-core)

gnc_add_test_with_guile(test-import-parse test-import-parse.c
  GENERIC_IMPORT_TEST_INCLUDE_DIRS GENERIC_IMPORT_TEST_LIBS
)
gnc_add_test(test-import-pending-matches test-import-pending-matches.cpp
  GENERIC_IMPORT_TEST_INCLUDE_DIRS GENERIC_IMPORT_TEST_LIBS
)

set(IMPORT_ACCOUNT_MATCHER_TEST_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common # for config.h
  ${CMAKE_SOURCE_DIR}/gnucash/import-export
  ${CMAKE_SOURCE_DIR}/libgnucash/engine
  ${CMAKE_SOURCE_DIR}/libgnucash/app-utils
  ${CMAKE_SOURCE_DIR}/gnucash/gnome-utils
  ${GTEST_INCLUDE_DIR}
  )

set(IMPORT_ACCOUNT_MATCHER_TEST_LIBS gnc-generic-import gnc-engine test-core gtest)
gnc_add_test(test-import-account-matcher gtest-import-account-matcher.cpp
  IMPORT_ACCOUNT_MATCHER_TEST_INCLUDE_DIRS IMPORT_ACCOUNT_MATCHER_TEST_LIBS)

set(gtest_import_backend_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common # for config.h
  ${CMAKE_SOURCE_DIR}/common
  ${CMAKE_SOURCE_DIR}/gnucash/import-export
  ${CMAKE_SOURCE_DIR}/libgnucash/app-utils
  ${CMAKE_SOURCE_DIR}/libgnucash/app-utils/mocks
  ${CMAKE_SOURCE_DIR}/libgnucash/core-utils
  ${CMAKE_SOURCE_DIR}/libgnucash/engine
  ${CMAKE_SOURCE_DIR}/libgnucash/engine/mocks
  ${GTEST_INCLUDE_DIR}
  ${GMOCK_INCLUDE_DIR}
)

set(gtest_import_backend_LIBS
  PkgConfig::GTK3
  ${Boost_LIBRARIES}
  ${GMODULE_LDFLAGS}
  ${GTHREAD_LDFLAGS}
  ${ICU4C_I18N_LDFLAGS}
  gmock
  gtest
)

set(gtest_import_backend_SOURCES
  gtest-import-backend.cpp
  ${CMAKE_SOURCE_DIR}/gnucash/import-export/import-backend.cpp
  ${CMAKE_SOURCE_DIR}/gnucash/import-export/import-settings.c
  ${CMAKE_SOURCE_DIR}/gnucash/import-export/import-utilities.c
  ${CMAKE_SOURCE_DIR}/libgnucash/engine/mocks/gmock-qofinstance.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/app-utils/mocks/gmock-gnc-prefs.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/engine/mocks/gmock-qofbook.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/engine/mocks/gmock-Account.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/engine/mocks/gmock-Transaction.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/engine/mocks/gmock-Split.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/engine/mocks/fake-qofquery.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/engine/gnc-numeric.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/engine/gnc-rational.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/engine/gnc-int128.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/engine/gnc-date.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/engine/gnc-datetime.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/engine/gnc-timezone.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/core-utils/gnc-locale-utils.cpp
)

gnc_add_test(test-import-backend "${gtest_import_backend_SOURCES}"
  gtest_import_backend_INCLUDE_DIRS gtest_import_backend_LIBS)

set_dist_list(test_generic_import_DIST
    CMakeLists.txt
    test-import-parse.c
    test-import-pending-matches.cpp
    gtest-import-account-matcher.cpp
    gtest-import-backend.cpp)
