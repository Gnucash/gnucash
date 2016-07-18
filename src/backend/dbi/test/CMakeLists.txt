
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
SET(BACKEND_DBI_TEST_LIBS gnc-backend-sql gncmod-engine gncmod-test-engine gnc-qof test-core ${Boost_REGEX_LIBRARY} ${LIBDBI_LIBRARY})

SET(test_dbi_backend_SOURCES
  test-backend-dbi.cpp
  test-backend-dbi-basic.cpp
  test-dbi-business-stuff.cpp
  test-dbi-stuff.cpp
  ../gnc-backend-dbi.cpp
  ../gnc-dbisqlconnection.cpp
)

# This test does not work on Win32
IF (NOT WIN32)
  GNC_ADD_TEST(test-backend-dbi "${test_dbi_backend_SOURCES}"
    BACKEND_DBI_TEST_INCLUDE_DIRS BACKEND_DBI_TEST_LIBS
  )

  TARGET_COMPILE_DEFINITIONS(test-backend-dbi PRIVATE
    TEST_MYSQL_URL=\"${TEST_MYSQL_URL}\"
    TEST_PGSQL_URL=\"${TEST_PGSQL_URL}\"
    DBI_TEST_XML_FILENAME=\"${CMAKE_CURRENT_SOURCE_DIR}/test-dbi.xml\"
  )
ENDIF()
