
set(BACKEND_DBI_TEST_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common # for config.h
  ${CMAKE_SOURCE_DIR}/libgnucash/core-utils
  ${CMAKE_SOURCE_DIR}/libgnucash/backend/dbi/test
  ${CMAKE_SOURCE_DIR}/libgnucash/backend/sql
  ${CMAKE_SOURCE_DIR}/libgnucash/engine
  ${CMAKE_SOURCE_DIR}/libgnucash/engine/test-core
  ${CMAKE_SOURCE_DIR}/common/test-core
  ${LIBDBI_INCLUDE_PATH}
  ${GLIB2_INCLUDE_DIRS}
)
set(BACKEND_DBI_TEST_LIBS gnc-backend-sql gncmod-engine gncmod-test-engine test-core ${Boost_REGEX_LIBRARY} ${LIBDBI_LIBRARY})

set(test_dbi_backend_SOURCES
  test-backend-dbi.cpp
  test-backend-dbi-basic.cpp
  test-dbi-business-stuff.cpp
  test-dbi-stuff.cpp
  ../gnc-backend-dbi.cpp
  ../gnc-dbisqlconnection.cpp
  ../gnc-dbisqlresult.cpp
)

set(test_dbi_backend_HEADERS test-dbi-business-stuff.h test-dbi-stuff.h)

set_dist_list(test_dbi_backend_DIST ${test_dbi_backend_SOURCES} ${test_dbi_backend_HEADERS} test-dbi.xml CMakeLists.txt )

# This test does not work on Win32
if (WITH_SQL AND NOT WIN32)
  gnc_add_test(test-backend-dbi "${test_dbi_backend_SOURCES}"
    BACKEND_DBI_TEST_INCLUDE_DIRS BACKEND_DBI_TEST_LIBS
  )

  target_compile_definitions(test-backend-dbi PRIVATE
    TEST_MYSQL_URL=\"${TEST_MYSQL_URL}\"
    TEST_PGSQL_URL=\"${TEST_PGSQL_URL}\"
    DBI_TEST_XML_FILENAME=\"${CMAKE_CURRENT_SOURCE_DIR}/test-dbi.xml\"
  )
endif()
