
SET(BACKEND_DBI_TEST_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/src # for config.h
  ${CMAKE_SOURCE_DIR}/src/core-utils
  ${CMAKE_SOURCE_DIR}/src/backend/dbi/test
  ${CMAKE_SOURCE_DIR}/src/backend/sql
  ${CMAKE_SOURCE_DIR}/src/libqof/qof # for qof.h
  ${CMAKE_SOURCE_DIR}/src/engine
  ${CMAKE_SOURCE_DIR}/src/engine/test-core
  ${CMAKE_SOURCE_DIR}/src/test-core
  ${LIBDBI_INCLUDE_PATH}
  ${GLIB2_INCLUDE_DIRS}
)
SET(BACKEND_DBI_TEST_LIBS gnc-backend-sql gncmod-backend-dbi gncmod-engine gncmod-test-engine gnc-qof test-core ${LIBDBI_LIBRARY})

SET(test_dbi_backend_SOURCES
  test-backend-dbi.c
  test-backend-dbi-basic.c
  test-dbi-business-stuff.c
  test-dbi-stuff.c
)

SET(test_dbi_backend_HEADERS test-dbi-business-stuff.h test-dbi-stuff.h)

SET_DIST_LIST(test_dbi_backend_DIST ${test_dbi_backend_SOURCES} ${test_dbi_backend_HEADERS} test-dbi.xml CMakeLists.txt Makefile.am)

# This test does not work on Win32
IF (WITH_SQL AND NOT WIN32)
  GNC_ADD_TEST(test-backend-dbi "${test_dbi_backend_SOURCES}"
    BACKEND_DBI_TEST_INCLUDE_DIRS BACKEND_DBI_TEST_LIBS
  )

  TARGET_COMPILE_DEFINITIONS(test-backend-dbi PRIVATE
    TEST_MYSQL_URL=\"${TEST_MYSQL_URL}\"
    TEST_PGSQL_URL=\"${TEST_PGSQL_URL}\"
    DBI_TEST_XML_FILENAME=\"${CMAKE_CURRENT_SOURCE_DIR}/test-dbi.xml\"
  )
ENDIF()
