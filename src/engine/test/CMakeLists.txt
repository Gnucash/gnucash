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

# Not via macro because of unique link requirements

ADD_EXECUTABLE(test-link EXCLUDE_FROM_ALL test-link.c)
TARGET_LINK_LIBRARIES(test-link gnc-qof gnc-core-utils)
ADD_TEST(NAME test-link COMMAND test-link)
ADD_DEPENDENCIES(check test-link)

#################################################

ADD_ENGINE_TEST(test-load-engine test-load-engine.c)
ADD_ENGINE_TEST(test-guid test-guid.cpp)
ADD_ENGINE_TEST(test-date test-date.cpp)
ADD_ENGINE_TEST(test-object test-object.c)
ADD_ENGINE_TEST(test-commodities test-commodities.cpp)

SET(test_engine_SOURCES
  test-engine.c
  utest-Account.cpp
  utest-Budget.c
  utest-Entry.c
  utest-Invoice.c
  utest-Split.cpp
  utest-Transaction.cpp
  test-engine-kvp-properties.c
  utest-gnc-pricedb.c
)

# This test does not run on Win32
IF (NOT WIN32)
  ADD_ENGINE_TEST(test-engine "${test_engine_SOURCES}")
  TARGET_COMPILE_OPTIONS(test-engine PRIVATE -Wno-write-strings)
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

############################
# This is a C test that needs GUILE environment variables set.
# It does not pass on Win32.
GNC_ADD_TEST_WITH_GUILE(test-scm-query test-scm-query.cpp ENGINE_TEST_INCLUDE_DIRS ENGINE_TEST_LIBS)
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
