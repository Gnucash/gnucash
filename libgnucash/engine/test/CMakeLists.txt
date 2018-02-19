# Common stuff

SET(ENGINE_TEST_INCLUDE_DIRS
  ${CMAKE_SOURCE_DIR}/libgnucash/backend/xml
  ${CMAKE_SOURCE_DIR}/libgnucash/engine
  ${CMAKE_SOURCE_DIR}/libgnucash/engine/test-core
  ${CMAKE_BINARY_DIR}/common # for config.h
  ${CMAKE_SOURCE_DIR}/common/test-core  # for unittest-support.h
  ${GLIB2_INCLUDE_DIRS}
)

SET(ENGINE_TEST_LIBS gncmod-engine gncmod-test-engine test-core ${LIBXML2_LDFLAGS} -lm)

MACRO(ADD_ENGINE_TEST _TARGET _SOURCE_FILES)
  GNC_ADD_TEST(${_TARGET} "${_SOURCE_FILES}" ENGINE_TEST_INCLUDE_DIRS ENGINE_TEST_LIBS)
ENDMACRO()

# Not via macro because of unique link requirements

ADD_EXECUTABLE(test-link EXCLUDE_FROM_ALL test-link.c)
TARGET_LINK_LIBRARIES(test-link gnc-core-utils)
ADD_TEST(NAME test-link COMMAND test-link)
ADD_DEPENDENCIES(check test-link)

#################################################

ADD_ENGINE_TEST(test-load-engine test-load-engine.c)
ADD_ENGINE_TEST(test-guid test-guid.cpp)
ADD_ENGINE_TEST(test-date test-date.cpp)
ADD_ENGINE_TEST(test-object test-object.c)
ADD_ENGINE_TEST(test-commodities test-commodities.cpp)

SET(test_qof_SOURCES
  test-gnc-date.c
  test-qof.c
  test-qofbook.c
  test-qofinstance.cpp
  test-qofobject.c
  test-qof-string-cache.c
)

SET(test_engine_SOURCES
  test-engine.c
  test-engine-kvp-properties.c
  test-gnc-uri-utils.c
  utest-Account.cpp
  utest-Budget.c
  utest-Entry.c
  utest-Invoice.c
  utest-Split.cpp
  utest-Transaction.cpp
  utest-gnc-pricedb.c
)

# This test does not run on Win32
IF (NOT WIN32)
  SET(SOURCES ${test_qof_SOURCES} ${CMAKE_SOURCE_DIR}/common/test-core/unittest-support.c)
  ADD_ENGINE_TEST(test-qof "${SOURCES}")
  TARGET_COMPILE_DEFINITIONS(test-qof PRIVATE TESTPROG=test_qof)

  SET(SOURCES ${test_engine_SOURCES} ${CMAKE_SOURCE_DIR}/common/test-core/unittest-support.c)
  ADD_ENGINE_TEST(test-engine "${SOURCES}")
  TARGET_COMPILE_OPTIONS(test-engine PRIVATE -Wno-write-strings)
  TARGET_COMPILE_DEFINITIONS(test-engine PRIVATE TESTPROG=test_engine)
ENDIF()

ADD_ENGINE_TEST(test-account-object test-account-object.cpp)
ADD_ENGINE_TEST(test-group-vs-book test-group-vs-book.cpp)
ADD_ENGINE_TEST(test-lots test-lots.cpp)
ADD_ENGINE_TEST(test-querynew test-querynew.c)
ADD_ENGINE_TEST(test-query test-query.cpp)
ADD_ENGINE_TEST(test-split-vs-account test-split-vs-account.cpp)
ADD_ENGINE_TEST(test-transaction-reversal test-transaction-reversal.cpp)
ADD_ENGINE_TEST(test-transaction-voiding test-transaction-voiding.cpp)
ADD_ENGINE_TEST(test-recurrence test-recurrence.c)
ADD_ENGINE_TEST(test-business test-business.c)
ADD_ENGINE_TEST(test-address test-address.c)
ADD_ENGINE_TEST(test-customer test-customer.c)
ADD_ENGINE_TEST(test-employee test-employee.c)
ADD_ENGINE_TEST(test-job test-job.c)
ADD_ENGINE_TEST(test-vendor test-vendor.c)

SET(test_numeric_SOURCES
  ${CMAKE_SOURCE_DIR}/libgnucash/engine/gnc-numeric.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/engine/gnc-rational.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/engine/gnc-int128.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/engine/test/test-numeric.cpp
)
ADD_ENGINE_TEST(test-numeric "${test_numeric_SOURCES}")

SET(MODULEPATH ${CMAKE_SOURCE_DIR}/libgnucash/engine)
SET(gtest_old_engine_LIBS
  gncmod-engine
  ${GLIB2_LDFLAGS}
  ${Boost_LIBRARIES}
  ${GTEST_LIB})

SET(gtest_qof_LIBS
  ${GLIB2_LDFLAGS}
  ${GOBJECT_LDFLAGS}
  ${GMODULE_LDFLAGS}
  ${GTHREAD_LDFLAGS}
  ${Boost_LIBRARIES}
  ${GTEST_LIB})

SET(gtest_engine_INCLUDES
  ${MODULEPATH}
  ${CMAKE_BINARY_DIR}/common # for config.h
  ${CMAKE_SOURCE_DIR}/common # for platform.h
  ${GLIB2_INCLUDE_DIRS}
  ${GTEST_INCLUDE_DIR}
  ../../../borrowed/libc
)

IF (WIN32)
  ADD_DEFINITIONS (-DOS_WIN32)
  SET (gtest_engine_win32_SOURCES
    ${MODULEPATH}/qof-win32.cpp
    ../../../borrowed/libc/strptime.c
  )
ENDIF (WIN32)

SET(test_gnc_guid_SOURCES
  ${MODULEPATH}/guid.cpp
  test-gnc-guid.cpp
  ${GTEST_SRC})
GNC_ADD_TEST(test-gnc-guid "${test_gnc_guid_SOURCES}"
  gtest_engine_INCLUDES gtest_old_engine_LIBS)

SET(test_kvp_value_SOURCES
  ${MODULEPATH}/kvp-value.cpp
  test-kvp-value.cpp
  test-kvp-frame.cpp
  ${GTEST_SRC})
GNC_ADD_TEST(test-kvp-value "${test_kvp_value_SOURCES}"
  gtest_engine_INCLUDES gtest_old_engine_LIBS)

SET(test_qofsession_SOURCES
  ${MODULEPATH}/qofsession.cpp
  test-qofsession.cpp
  ${GTEST_SRC})
GNC_ADD_TEST(test-qofsession "${test_qofsession_SOURCES}"
  gtest_engine_INCLUDES gtest_old_engine_LIBS)

SET(test_gnc_int128_SOURCES
  ${MODULEPATH}/gnc-int128.cpp
  gtest-gnc-int128.cpp
  ${GTEST_SRC})
GNC_ADD_TEST(test-gnc-int128 "${test_gnc_int128_SOURCES}"
  gtest_engine_INCLUDES gtest_qof_LIBS)

SET(test_gnc_rational_SOURCES
  ${MODULEPATH}/gnc-rational.cpp
  ${MODULEPATH}/gnc-numeric.cpp
  ${MODULEPATH}/gnc-int128.cpp
  ${MODULEPATH}/gnc-datetime.cpp
  ${MODULEPATH}/gnc-timezone.cpp
  ${MODULEPATH}/gnc-date.cpp
  ${MODULEPATH}/qoflog.cpp
  ${gtest_engine_win32_SOURCES}
  gtest-gnc-rational.cpp
  ${GTEST_SRC})

GNC_ADD_TEST(test-gnc-rational "${test_gnc_rational_SOURCES}"
  gtest_engine_INCLUDES gtest_qof_LIBS)

SET(test_gnc_numeric_SOURCES
  ${MODULEPATH}/gnc-rational.cpp
  ${MODULEPATH}/gnc-int128.cpp
  ${MODULEPATH}/gnc-numeric.cpp
  ${MODULEPATH}/gnc-datetime.cpp
  ${MODULEPATH}/gnc-timezone.cpp
  ${MODULEPATH}/gnc-date.cpp
  ${MODULEPATH}/qoflog.cpp
  ${gtest_engine_win32_SOURCES}
  gtest-gnc-numeric.cpp
  ${GTEST_SRC})
GNC_ADD_TEST(test-gnc-numeric "${test_gnc_numeric_SOURCES}"
  gtest_engine_INCLUDES gtest_qof_LIBS)

SET(test_gnc_timezone_SOURCES
  ${MODULEPATH}/gnc-timezone.cpp
  gtest-gnc-timezone.cpp
  ${GTEST_SRC})
GNC_ADD_TEST(test-gnc-timezone "${test_gnc_timezone_SOURCES}"
  gtest_engine_INCLUDES gtest_old_engine_LIBS)

SET(test_gnc_datetime_SOURCES
  ${MODULEPATH}/gnc-datetime.cpp
  ${MODULEPATH}/gnc-timezone.cpp
  ${MODULEPATH}/gnc-date.cpp
  ${MODULEPATH}/qoflog.cpp
  ${gtest_engine_win32_SOURCES}
  gtest-gnc-datetime.cpp
  ${GTEST_SRC})
GNC_ADD_TEST(test-gnc-datetime "${test_gnc_datetime_SOURCES}"
  gtest_engine_INCLUDES gtest_qof_LIBS)

SET(test_import_map_SOURCES
  gtest-import-map.cpp
  ${GTEST_SRC})
GNC_ADD_TEST(test-import-map "${test_import_map_SOURCES}"
  gtest_engine_INCLUDES gtest_old_engine_LIBS)

############################
# This is a C test that needs GUILE environment variables set.
# It does not pass on Win32.
GNC_ADD_TEST_WITH_GUILE(test-scm-query test-scm-query.cpp ENGINE_TEST_INCLUDE_DIRS ENGINE_TEST_LIBS)
############################


SET(engine_test_SCHEME
  test-account.scm
  test-create-account.scm
  test-test-extras.scm
  test-split.scm
  )

#LIST(APPEND engine_test_SCHEME  test-scm-query-import.scm) Fails

SET(GUILE_DEPENDS      scm-gnc-module scm-app-utils scm-scm scm-engine)

GNC_ADD_SCHEME_TARGETS(scm-test-engine-extras
  "test-extras.scm"
  "gnucash/engine/test/"
  "${GUILE_DEPENDS}"
  FALSE
  )

GNC_ADD_SCHEME_TARGETS(scm-test-engine
  "${engine_test_SCHEME}"
  ""
  "${GUILE_DEPENDS};scm-test-engine-extras"
  FALSE
  )

add_dependencies(check scm-test-engine)
GNC_ADD_SCHEME_TESTS("${engine_test_SCHEME}")

SET(test_engine_SOURCES_DIST
        dummy.cpp
        gtest-gnc-int128.cpp
        gtest-gnc-rational.cpp
        gtest-gnc-numeric.cpp
        gtest-gnc-timezone.cpp
        gtest-gnc-datetime.cpp
        gtest-import-map.cpp
        test-account-object.cpp
        test-address.c
        test-business.c
        test-commodities.cpp
        test-customer.c
        test-date.cpp
        test-employee.c
        test-engine-kvp-properties.c
        test-engine.c
        test-gnc-date.c
        test-gnc-guid.cpp
	test-gnc-uri-utils.c
        test-group-vs-book.cpp
        test-guid.cpp
        test-job.c
        test-kvp-value.cpp
        test-kvp-frame.cpp
        test-link.c
        test-load-engine.c
        test-lots.cpp
        test-numeric.cpp
        test-object.c
        test-qof.c
        test-qofbook.c
        test-qofinstance.cpp
        test-qofobject.c
        test-qofsession.cpp
        test-qof-string-cache.c
        test-query.cpp
        test-querynew.c
        test-recurrence.c
        test-scm-query.cpp
        test-split-vs-account.cpp
        test-transaction-reversal.cpp
        test-transaction-voiding.cpp
        test-vendor.c
        utest-Account.cpp
        utest-Budget.c
        utest-Entry.c
        utest-Invoice.c
        utest-Split.cpp
        utest-Transaction.cpp
        utest-gnc-pricedb.c
)

SET(test_engine_SCHEME_DIST
        test-account.scm
        test-create-account.scm
        test-extras.scm
        test-scm-query-import.scm
        test-split.scm
        test-test-extras.scm
)

SET(test_engine_EXTRA_DIST
        README
)

SET_DIST_LIST(test_engine_DIST CMakeLists.txt 
        ${test_engine_SOURCES_DIST} ${test_engine_SCHEME_DIST} ${test_engine_EXTRA_DIST})
