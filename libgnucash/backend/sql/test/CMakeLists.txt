
set(BACKEND_SQL_TEST_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common # for config.h
  ${CMAKE_SOURCE_DIR}/libgnucash/backend/sql
  ${CMAKE_SOURCE_DIR}/libgnucash/engine
  ${CMAKE_SOURCE_DIR}/common/test-core
  ${GLIB2_INCLUDE_DIRS}
)

set(test_backend_sql_SOURCES test-sqlbe.cpp utest-gnc-backend-sql.cpp)

set(BACKEND_SQL_TEST_LIBS gnc-backend-sql gncmod-engine test-core)

set_dist_list(test_backend_sql_DIST ${test_backend_sql_SOURCES} CMakeLists.txt
  test-column-types.cpp)

if(WITH_SQL)
  # This test does not actually do anything.
  gnc_add_test(test-column-types test-column-types.cpp
    BACKEND_SQL_TEST_INCLUDE_DIRS BACKEND_SQL_TEST_LIBS
  )

  gnc_add_test(test-sqlbe "${test_backend_sql_SOURCES}"
    BACKEND_SQL_TEST_INCLUDE_DIRS BACKEND_SQL_TEST_LIBS
  )
  target_compile_definitions(test-sqlbe PRIVATE TESTPROG=test_sqlbe)
endif()
