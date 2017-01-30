SET(TEST_QOF_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/src # for config.h
  ${CMAKE_SOURCE_DIR}/src/test-core  # for unittest-support.h
  ${CMAKE_SOURCE_DIR}/src/libqof/qof # for qof.h
  ${GLIB2_INCLUDE_DIRS}
)

SET(TEST_QOF_LIBS
  gnc-qof test-core ${GLIB2_LDFLAGS} -lm
)

SET(test_qof_SOURCES
  test-gnc-date.c
  test-qof.c
  test-qofbook.c
  test-qofinstance.cpp
  test-qofobject.c
  test-qof-string-cache.c
  ${CMAKE_SOURCE_DIR}/src/test-core/unittest-support.c
)

SET(TEST_ENGINE_INCLUDE_DIRS
  ${TEST_QOF_INCLUDE_DIRS}
  ${CMAKE_SOURCE_DIR}/src/engine
  ${CMAKE_SOURCE_DIR}/src/engine/test-core
  ${CMAKE_BINARY_DIR}/src # for config.h
  )

SET(TEST_ENGINE_LIBS
  ${TEST_QOF_LIBS}
  gncmod-test-engine
  )

SET(test_numeric_SOURCES
  ${CMAKE_SOURCE_DIR}/src/engine/cashobjects.c
  ${CMAKE_SOURCE_DIR}/src/engine/test-core/test-engine-stuff.cpp
  ${CMAKE_SOURCE_DIR}/src/libqof/qof/gnc-numeric.cpp
  ${CMAKE_SOURCE_DIR}/src/libqof/qof/gnc-rational.cpp
  ${CMAKE_SOURCE_DIR}/src/libqof/qof/gnc-int128.cpp
  ${CMAKE_SOURCE_DIR}/src/libqof/qof/test/test-numeric.cpp
)


GNC_ADD_TEST(test-numeric "${test_numeric_SOURCES}" TEST_ENGINE_INCLUDE_DIRS TEST_ENGINE_LIBS)

# This test does not on Win32. Worse, it causes a dialog box to
# pop up due to an assertion. This interferes with running the tests
# unattended.
IF (NOT WIN32)
  GNC_ADD_TEST(test-qof "${test_qof_SOURCES}" TEST_QOF_INCLUDE_DIRS TEST_QOF_LIBS)
  TARGET_COMPILE_DEFINITIONS(test-qof PRIVATE TESTPROG=test_qof)
  SET(MODULEPATH ${CMAKE_SOURCE_DIR}/src/libqof/qof)
  SET(gtest_qof_LIBS gnc-qof ${GLIB2_LDFLAGS} ${Boost_LIBRARIES} ${GTEST_LIB})
  SET(gtest_qof_INCLUDES
    ${MODULEPATH}
    ${GLIB2_INCLUDE_DIRS}
    ${GTEST_INCLUDE_DIR})

  SET(test_gnc_guid_SOURCES
    ${MODULEPATH}/guid.cpp
    test-gnc-guid.cpp
    ${GTEST_SRC})
  GNC_ADD_TEST(test-gnc-guid "${test_gnc_guid_SOURCES}"
    gtest_qof_INCLUDES gtest_qof_LIBS)

  SET(test_kvp_value_SOURCES
    ${MODULEPATH}/kvp-value.cpp
    test-kvp-value.cpp
    test-kvp-frame.cpp
    ${GTEST_SRC})
  GNC_ADD_TEST(test-kvp-value "${test_kvp_value_SOURCES}"
    gtest_qof_INCLUDES gtest_qof_LIBS)

  SET(test_qofsession_SOURCES
    ${MODULEPATH}/qofsession.cpp
    test-qofsession.cpp
    ${GTEST_SRC})
  GNC_ADD_TEST(test-qofsession "${test_qofsession_SOURCES}"
    gtest_qof_INCLUDES gtest_qof_LIBS)

  SET(test_gnc_int128_SOURCES
    ${MODULEPATH}/gnc-int128.cpp
    gtest-gnc-int128.cpp
    ${GTEST_SRC})
  GNC_ADD_TEST(test-gnc-int128 "${test_gnc_int128_SOURCES}"
    gtest_qof_INCLUDES gtest_qof_LIBS)

  SET(test_gnc_rational_SOURCES
    ${MODULEPATH}/gnc-rational.cpp
    gtest-gnc-rational.cpp
    ${GTEST_SRC})
  GNC_ADD_TEST(test-gnc-rational "${test_gnc_rational_SOURCES}"
    gtest_qof_INCLUDES gtest_qof_LIBS)

  SET(test_gnc_timezone_SOURCES
    ${MODULEPATH}/gnc-timezone.cpp
    gtest-gnc-timezone.cpp
    ${GTEST_SRC})
  GNC_ADD_TEST(test-gnc-timezone "${test_gnc_timezone_SOURCES}"
    gtest_qof_INCLUDES gtest_qof_LIBS)

  SET(test_gnc_datetime_SOURCES
    ${MODULEPATH}/gnc-datetime.cpp
    gtest-gnc-datetime.cpp
    ${GTEST_SRC})
  GNC_ADD_TEST(test-gnc-datetime "${test_gnc_datetime_SOURCES}"
    gtest_qof_INCLUDES gtest_qof_LIBS)
ENDIF()
