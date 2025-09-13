
set(BACKEND_DBI_TEST_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common # for config.h
  ${CMAKE_SOURCE_DIR}/libgnucash/core-utils
  ${CMAKE_SOURCE_DIR}/libgnucash/backend/dbi/test
  ${CMAKE_SOURCE_DIR}/libgnucash/backend/sql
  ${CMAKE_SOURCE_DIR}/libgnucash/engine
  ${CMAKE_SOURCE_DIR}/libgnucash/engine/test-core
  ${CMAKE_SOURCE_DIR}/common/test-core
  ${LIBDBI_INCLUDE_PATH}
)
set(BACKEND_DBI_TEST_LIBS
  gnc-backend-sql
  gnc-engine
  gnc-test-engine
  test-core
  ${Boost_REGEX_LIBRARY}
  ${LIBDBI_LIBRARY}
  $<$<BOOL:${WIN32}>:${WINSOCK_LIB}>
  $<$<BOOL:${WIN32}>:${WS2_LIB}>
)

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

if (WITH_SQL)
  gnc_add_test(test-backend-dbi "${test_dbi_backend_SOURCES}"
    BACKEND_DBI_TEST_INCLUDE_DIRS BACKEND_DBI_TEST_LIBS
  )
  if (DEFINED ENV{TMPDIR})
    set(temp_dir "$ENV{TMPDIR}")
  elseif (DEFINED ENV{TMP})
    set(temp_dir "$ENV{TMP}")
  elseif (DEFINED ENV{TEMP})
    set(temp_dir "$ENV{TEMP}")
  else()
    set(temp_dir "${CMAKE_TOP_BUILD_DIR}/tmp")
    file(MAKE_DIRECTORY ${temp_dir})
  endif()

  if (MINGW64)
    string(REGEX REPLACE "\\\\" "/" temp_dir ${temp_dir})
  endif()

  target_compile_definitions(test-backend-dbi PRIVATE
    DBI_TEST_XML_FILENAME=\"${CMAKE_CURRENT_SOURCE_DIR}/test-dbi.xml\"
    TEMPDIR=\"${temp_dir}\"
    G_LOG_DOMAIN=\"gnc.backend.dbi\"
  )
endif()
