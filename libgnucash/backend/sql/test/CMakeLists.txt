
SET(BACKEND_SQL_TEST_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common # for config.h
  ${CMAKE_SOURCE_DIR}/libgnucash/backend/sql
  ${CMAKE_SOURCE_DIR}/libgnucash/engine
  ${CMAKE_SOURCE_DIR}/common/test-core
  ${GLIB2_INCLUDE_DIRS}
)

SET(test_backend_sql_SOURCES test-sqlbe.cpp utest-gnc-backend-sql.cpp)

SET(BACKEND_SQL_TEST_LIBS gnc-backend-sql gncmod-engine test-core)

SET_DIST_LIST(test_backend_sql_DIST ${test_backend_sql_SOURCES} CMakeLists.txt
  test-column-types.cpp)

IF(WITH_SQL)
  # This test does not actually do anything.
  GNC_ADD_TEST(test-column-types test-column-types.cpp
    BACKEND_SQL_TEST_INCLUDE_DIRS BACKEND_SQL_TEST_LIBS
  )

  GNC_ADD_TEST(test-sqlbe "${test_backend_sql_SOURCES}"
    BACKEND_SQL_TEST_INCLUDE_DIRS BACKEND_SQL_TEST_LIBS
  )
  TARGET_COMPILE_DEFINITIONS(test-sqlbe PRIVATE TESTPROG=test_sqlbe)
ENDIF(WITH_SQL)
