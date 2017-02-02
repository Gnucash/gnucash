# Common stuff

SET(ENGINE_TEST_INCLUDE_DIRS
  ${CMAKE_SOURCE_DIR}/src/backend/xml
  ${CMAKE_SOURCE_DIR}/src/engine
  ${CMAKE_SOURCE_DIR}/src/engine/test-core
  ${CMAKE_SOURCE_DIR}/src/libqof/qof # for qof.h
  ${CMAKE_BINARY_DIR}/src # for config.h
  ${CMAKE_SOURCE_DIR}/src/test-core  # for unittest-support.h
  ${GLIB2_INCLUDE_DIRS}
)

SET(ENGINE_TEST_LIBS gncmod-engine gnc-qof gncmod-test-engine test-core ${LIBXML2_LDFLAGS})

MACRO(ADD_ENGINE_TEST _TARGET _SOURCE_FILES)
  GNC_ADD_TEST(${_TARGET} "${_SOURCE_FILES}" ENGINE_TEST_INCLUDE_DIRS ENGINE_TEST_LIBS)
ENDMACRO()

CONFIGURE_FILE(test-scm-query-import.in test-scm-query-import @ONLY)
CONFIGURE_FILE(test-create-account.in test-create-account @ONLY)
# Not via macro because of unique link requirements

ADD_EXECUTABLE(test-link EXCLUDE_FROM_ALL test-link.c)
TARGET_LINK_LIBRARIES(test-link gnc-qof gnc-core-utils)
ADD_TEST(NAME test-link COMMAND test-link)
ADD_DEPENDENCIES(check test-link)

#################################################

ADD_ENGINE_TEST(test-load-engine test-load-engine.c)
ADD_ENGINE_TEST(test-guid test-guid.c)
ADD_ENGINE_TEST(test-numeric test-numeric.c)
ADD_ENGINE_TEST(test-date test-date.c)
ADD_ENGINE_TEST(test-object test-object.c)
ADD_ENGINE_TEST(test-commodities test-commodities.c)

SET(test_engine_SOURCES
  test-engine.c
  utest-Account.c
  utest-Budget.c
  utest-Entry.c
  utest-Invoice.c
  utest-Split.c
  utest-Transaction.c
)

# This test does not run on Win32
IF (NOT WIN32)
  ADD_ENGINE_TEST(test-engine "${test_engine_SOURCES}")
  TARGET_COMPILE_OPTIONS(test-engine PRIVATE -Wno-write-strings)
ENDIF()

ADD_ENGINE_TEST(test-account-object test-account-object.c)
ADD_ENGINE_TEST(test-group-vs-book test-group-vs-book.c)
ADD_ENGINE_TEST(test-lots test-lots.c)
ADD_ENGINE_TEST(test-querynew test-querynew.c)
ADD_ENGINE_TEST(test-query test-query.c)
ADD_ENGINE_TEST(test-split-vs-account test-split-vs-account.c)
ADD_ENGINE_TEST(test-transaction-reversal test-transaction-reversal.c)
ADD_ENGINE_TEST(test-transaction-voiding test-transaction-voiding.c)
ADD_ENGINE_TEST(test-recurrence test-recurrence.c)
ADD_ENGINE_TEST(test-business test-business.c)
ADD_ENGINE_TEST(test-address test-address.c)
ADD_ENGINE_TEST(test-customer test-customer.c)
ADD_ENGINE_TEST(test-employee test-employee.c)
ADD_ENGINE_TEST(test-job test-job.c)
ADD_ENGINE_TEST(test-vendor test-vendor.c)

############################
# This is a C test that needs GUILE environment variables set.
# It does not pass on Win32.
GNC_ADD_TEST_WITH_GUILE(test-scm-query test-scm-query.c ENGINE_TEST_INCLUDE_DIRS ENGINE_TEST_LIBS)
############################

GNC_ADD_SCHEME_TEST(scm-test-account test-account.scm)
GNC_ADD_SCHEME_TEST(scm-test-create-account test-create-account.scm)
GNC_ADD_SCHEME_TEST(scm-test-test-extra test-test-extras.scm)
GNC_ADD_SCHEME_TEST(scm-test-split test-split.scm)
# This check not run in autotools. Doesn't pass.
#GNC_ADD_SCHEME_TEST(scm-test-scm-query-import test-scm-query-import.scm)

SET(engine_test_SCHEME test-extras.scm)

SET(GUILE_MODULES      "")
SET(GUILE_LOAD_DIRS    src/app-utils src/scm)
SET(GUILE_LIBRARY_DIRS "")
SET(GUILE_DEPENDS      scm-app-utils scm-scm)

GNC_ADD_SCHEME_TARGETS(scm-test-engine
  "${engine_test_SCHEME}"
  gnucash/engine/test
  "${GUILE_MODULES}"
  "${GUILE_LOAD_DIRS}"
  "${GUILE_LIBRARY_DIRS}"
  "${GUILE_DEPENDS}"
  FALSE
)

SET(test_engine_SOURCES_DIST
        test-account-object.c
        test-address.c
        test-business.c
        test-commodities.c
        test-customer.c
        test-date.c
        test-employee.c
        test-engine.c
        test-group-vs-book.c
        test-guid.c
        test-job.c
        test-link.c
        test-load-engine.c
        test-lots.c
        test-numeric.c
        test-object.c
        test-query.c
        test-querynew.c
        test-recurrence.c
        test-scm-query.c
        test-split-vs-account.c
        test-transaction-reversal.c
        test-transaction-voiding.c
        test-vendor.c
        utest-Account.c
        utest-Budget.c
        utest-Entry.c
        utest-Invoice.c
        utest-Split.c
        utest-Transaction.c
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
        test-create-account.in
        test-scm-query-import.in
)

SET_DIST_LIST(test_engine_DIST CMakeLists.txt Makefile.am
        ${test_engine_SOURCES_DIST} ${test_engine_SCHEME_DIST} ${test_engine_EXTRA_DIST})
