# Common stuff

set(ENGINE_TEST_INCLUDE_DIRS
  ${CMAKE_SOURCE_DIR}/libgnucash/backend/xml
  ${CMAKE_SOURCE_DIR}/libgnucash/engine
  ${CMAKE_SOURCE_DIR}/libgnucash/engine/test-core
  ${CMAKE_BINARY_DIR}/common # for config.h
  ${CMAKE_SOURCE_DIR}/common/test-core  # for unittest-support.h
  ${GLIB2_INCLUDE_DIRS}
)

set(ENGINE_TEST_LIBS gncmod-engine gncmod-test-engine test-core ${LIBXML2_LDFLAGS} -lm)

macro(add_engine_test _TARGET _SOURCE_FILES)
  gnc_add_test(${_TARGET} "${_SOURCE_FILES}" ENGINE_TEST_INCLUDE_DIRS ENGINE_TEST_LIBS)
endmacro()

# Not via macro because of unique link requirements

add_executable(test-link EXCLUDE_FROM_ALL test-link.c)
target_link_libraries(test-link gnc-core-utils)
add_test(NAME test-link COMMAND test-link CONFIGURATIONS Debug;Release)
add_dependencies(check test-link)

#################################################

add_engine_test(test-load-engine test-load-engine.c)
add_engine_test(test-guid test-guid.cpp)
add_engine_test(test-object test-object.c)
add_engine_test(test-commodities test-commodities.cpp)

set(test_qof_SOURCES
  test-gnc-date.c
  test-qof.c
  test-qofbook.c
  test-qofinstance.cpp
  test-qofobject.c
  test-qof-string-cache.c
)

set(test_engine_SOURCES
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
if (NOT WIN32)
  set(SOURCES ${test_qof_SOURCES} ${CMAKE_SOURCE_DIR}/common/test-core/unittest-support.c)
  add_engine_test(test-qof "${SOURCES}")
  target_compile_definitions(test-qof PRIVATE TESTPROG=test_qof)

  set(SOURCES ${test_engine_SOURCES} ${CMAKE_SOURCE_DIR}/common/test-core/unittest-support.c)
  add_engine_test(test-engine "${SOURCES}")
  target_compile_options(test-engine PRIVATE -Wno-write-strings)
  target_compile_definitions(test-engine PRIVATE TESTPROG=test_engine)
endif()

add_engine_test(test-account-object test-account-object.cpp)
add_engine_test(test-group-vs-book test-group-vs-book.cpp)
add_engine_test(test-lots test-lots.cpp)
add_engine_test(test-querynew test-querynew.c)
add_engine_test(test-query test-query.cpp)
add_engine_test(test-split-vs-account test-split-vs-account.cpp)
add_engine_test(test-transaction-reversal test-transaction-reversal.cpp)
add_engine_test(test-transaction-voiding test-transaction-voiding.cpp)
add_engine_test(test-recurrence test-recurrence.c)
add_engine_test(test-business test-business.c)
add_engine_test(test-address test-address.c)
add_engine_test(test-customer test-customer.c)
add_engine_test(test-employee test-employee.c)
add_engine_test(test-job test-job.c)
add_engine_test(test-vendor test-vendor.c)

set(test_numeric_SOURCES
  ${CMAKE_SOURCE_DIR}/libgnucash/engine/gnc-numeric.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/engine/gnc-rational.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/engine/gnc-int128.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/engine/test/test-numeric.cpp
)
add_engine_test(test-numeric "${test_numeric_SOURCES}")

set(MODULEPATH ${CMAKE_SOURCE_DIR}/libgnucash/engine)
set(gtest_old_engine_LIBS
  gncmod-engine
  ${GLIB2_LDFLAGS}
  ${Boost_LIBRARIES}
  ${GTEST_LIB})

set(gtest_qof_LIBS
  ${GLIB2_LDFLAGS}
  ${GOBJECT_LDFLAGS}
  ${GMODULE_LDFLAGS}
  ${GTHREAD_LDFLAGS}
  ${ICU4C_I18N_LDFLAGS}
  ${Boost_LIBRARIES}
  ${GTEST_LIB})

set(gtest_engine_INCLUDES
  ${MODULEPATH}
  ${CMAKE_SOURCE_DIR}/libgnucash/core-utils
  ${CMAKE_BINARY_DIR}/common # for config.h
  ${CMAKE_SOURCE_DIR}/common # for platform.h
  ${GLIB2_INCLUDE_DIRS}
  ${GTEST_INCLUDE_DIR}
  ../../../borrowed/libc
)

if (WIN32)
  add_definitions (-DOS_WIN32)
  set (gtest_engine_win32_SOURCES
    ${MODULEPATH}/qof-win32.cpp
    ../../../borrowed/libc/strptime.c
  )
endif()

set(test_gnc_guid_SOURCES
  ${MODULEPATH}/guid.cpp
  test-gnc-guid.cpp)
gnc_add_test(test-gnc-guid "${test_gnc_guid_SOURCES}"
  gtest_engine_INCLUDES gtest_old_engine_LIBS)

set(test_kvp_value_SOURCES
  ${MODULEPATH}/kvp-value.cpp
  test-kvp-value.cpp
  test-kvp-frame.cpp)
gnc_add_test(test-kvp-value "${test_kvp_value_SOURCES}"
  gtest_engine_INCLUDES gtest_old_engine_LIBS)

set(test_qofsession_SOURCES
  ${MODULEPATH}/qofsession.cpp
  test-qofsession.cpp)
gnc_add_test(test-qofsession "${test_qofsession_SOURCES}"
  gtest_engine_INCLUDES gtest_old_engine_LIBS)

set(test_gnc_int128_SOURCES
  ${MODULEPATH}/gnc-int128.cpp
  gtest-gnc-int128.cpp)
gnc_add_test(test-gnc-int128 "${test_gnc_int128_SOURCES}"
  gtest_engine_INCLUDES gtest_qof_LIBS)

set(test_gnc_rational_SOURCES
  ${MODULEPATH}/gnc-rational.cpp
  ${MODULEPATH}/gnc-numeric.cpp
  ${MODULEPATH}/gnc-int128.cpp
  ${MODULEPATH}/gnc-datetime.cpp
  ${MODULEPATH}/gnc-timezone.cpp
  ${MODULEPATH}/gnc-date.cpp
  ${MODULEPATH}/qoflog.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/core-utils/gnc-locale-utils.cpp
  ${gtest_engine_win32_SOURCES}
  gtest-gnc-rational.cpp)

gnc_add_test(test-gnc-rational "${test_gnc_rational_SOURCES}"
  gtest_engine_INCLUDES gtest_qof_LIBS)

set(test_gnc_numeric_SOURCES
  ${MODULEPATH}/gnc-rational.cpp
  ${MODULEPATH}/gnc-int128.cpp
  ${MODULEPATH}/gnc-numeric.cpp
  ${MODULEPATH}/gnc-datetime.cpp
  ${MODULEPATH}/gnc-timezone.cpp
  ${MODULEPATH}/gnc-date.cpp
  ${MODULEPATH}/qoflog.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/core-utils/gnc-locale-utils.cpp
  ${gtest_engine_win32_SOURCES}
  gtest-gnc-numeric.cpp)
gnc_add_test(test-gnc-numeric "${test_gnc_numeric_SOURCES}"
  gtest_engine_INCLUDES gtest_qof_LIBS)

set(test_gnc_timezone_SOURCES
  ${MODULEPATH}/gnc-timezone.cpp
  gtest-gnc-timezone.cpp)
gnc_add_test(test-gnc-timezone "${test_gnc_timezone_SOURCES}"
  gtest_engine_INCLUDES gtest_old_engine_LIBS)

set(test_gnc_datetime_SOURCES
  ${MODULEPATH}/gnc-datetime.cpp
  ${MODULEPATH}/gnc-timezone.cpp
  ${MODULEPATH}/gnc-date.cpp
  ${MODULEPATH}/qoflog.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/core-utils/gnc-locale-utils.cpp
  ${gtest_engine_win32_SOURCES}
  gtest-gnc-datetime.cpp)
gnc_add_test(test-gnc-datetime "${test_gnc_datetime_SOURCES}"
  gtest_engine_INCLUDES gtest_qof_LIBS)

set(test_import_map_SOURCES
  gtest-import-map.cpp)
gnc_add_test(test-import-map "${test_import_map_SOURCES}"
  gtest_engine_INCLUDES gtest_old_engine_LIBS)

set(test_qofquerycore_SOURCES
gtest-qofquerycore.cpp)
gnc_add_test(test-qofquerycore "${test_qofquerycore_SOURCES}"
  gtest_engine_INCLUDES gtest_old_engine_LIBS)

############################
# This is a C test that needs GUILE environment variables set.
# It does not pass on Win32.
gnc_add_test_with_guile(test-scm-query test-scm-query.cpp ENGINE_TEST_INCLUDE_DIRS ENGINE_TEST_LIBS)
############################


set(engine_test_SCHEME
  test-account.scm
  test-create-account.scm
  test-split.scm
  )

#list(APPEND engine_test_SCHEME  test-scm-query-import.scm) Fails

set(GUILE_DEPENDS      scm-gnc-module scm-app-utils scm-scm scm-engine)

gnc_add_scheme_targets(scm-test-engine-extras
  "test-extras.scm"
  "gnucash/engine/test/"
  "${GUILE_DEPENDS}"
  FALSE
  )

if (HAVE_SRFI64)
  gnc_add_scheme_targets (scm-srfi64-extras
    "srfi64-extras.scm"
    "gnucash/engine/test/"
    "${GUILE_DEPENDS}"
    FALSE
    )

  set(srfi64_extras_SCHEME_DIST
    srfi64-extras.scm
    )

  set (scm_tests_with_srfi64_SOURCES
    test-business-core.scm
    )

  gnc_add_scheme_tests("${scm_tests_with_srfi64_SOURCES}")
  add_dependencies(check scm-srfi64-extras)
endif()

gnc_add_scheme_targets(scm-test-engine
  "${engine_test_SCHEME}"
  ""
  "${GUILE_DEPENDS};scm-test-engine-extras"
  FALSE
  )

add_dependencies(check scm-test-engine)
gnc_add_scheme_tests("${engine_test_SCHEME}")

set(test_engine_SOURCES_DIST
        dummy.cpp
        gtest-gnc-int128.cpp
        gtest-gnc-rational.cpp
        gtest-gnc-numeric.cpp
        gtest-gnc-timezone.cpp
        gtest-gnc-datetime.cpp
        gtest-import-map.cpp
        gtest-qofquerycore.cpp
        test-account-object.cpp
        test-address.c
        test-business.c
        test-commodities.cpp
        test-customer.c
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

set(test_engine_SCHEME_DIST
        test-account.scm
        test-create-account.scm
        test-extras.scm
        test-scm-query-import.scm
        test-split.scm
        test-business-core.scm
)

set(test_engine_EXTRA_DIST
        README
)

set_dist_list(test_engine_DIST CMakeLists.txt
        ${srfi64_extras_SCHEME_DIST}
        ${test_engine_SOURCES_DIST} ${test_engine_SCHEME_DIST} ${test_engine_EXTRA_DIST})
