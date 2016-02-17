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
#  test-kvp-frame.cpp  This now use Google Test
  test-qofobject.c
  test-qofsession.cpp
  test-qof-string-cache.c
  test-gnc-guid.cpp
  ${CMAKE_SOURCE_DIR}/src/test-core/unittest-support.c
)

# This test does not on Win32. Worse, it causes a dialog box to
# pop up due to an assertion. This interferes with running the tests
# unattended.
IF (NOT WIN32)
  GNC_ADD_TEST(test-qof "${test_qof_SOURCES}" TEST_QOF_INCLUDE_DIRS TEST_QOF_LIBS)
  TARGET_COMPILE_DEFINITIONS(test-qof PRIVATE TESTPROG=test_qof)
ENDIF()
