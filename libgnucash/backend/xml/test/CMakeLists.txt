# Common stuff
ADD_SUBDIRECTORY(test-files)

SET(XML_TEST_INCLUDE_DIRS
  ${CMAKE_SOURCE_DIR}/libgnucash/backend/xml
  ${CMAKE_SOURCE_DIR}/libgnucash/backend/xml/test
  ${CMAKE_SOURCE_DIR}/libgnucash/engine
  ${CMAKE_SOURCE_DIR}/libgnucash/engine/test-core
  ${CMAKE_BINARY_DIR}/common # for config.h
  ${CMAKE_SOURCE_DIR}/common/test-core  # for unittest-support.h
  ${GLIB2_INCLUDE_DIRS}
  ${LIBXML2_INCLUDE_DIRS}
  ${ZLIB_INCLUDE_DIRS}
)


SET(XML_TEST_LIBS gncmod-engine gncmod-test-engine test-core ${LIBXML2_LDFLAGS} -lz)

FUNCTION(ADD_XML_TEST _TARGET _SOURCE_FILES)
  GNC_ADD_TEST(${_TARGET} "${_SOURCE_FILES}" XML_TEST_INCLUDE_DIRS XML_TEST_LIBS ${ARGN})
  TARGET_COMPILE_OPTIONS(${_TARGET} PRIVATE -DU_SHOW_CPLUSPLUS_API=0)
ENDFUNCTION()


################################

SET(test_backend_xml_base_SOURCES
  ${CMAKE_SOURCE_DIR}/libgnucash/backend/xml/sixtp-dom-parsers.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/backend/xml/sixtp-dom-generators.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/backend/xml/sixtp-utils.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/backend/xml/sixtp.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/backend/xml/sixtp-stack.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/backend/xml/sixtp-to-dom-parser.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/backend/xml/gnc-xml-helper.cpp
)

## the xml backend is now a GModule - this test does
## not load it as a module and cannot link to it
## and remain portable.

SET(test_backend_xml_module_SOURCES
  ${test_backend_xml_base_SOURCES}
  ${CMAKE_SOURCE_DIR}/libgnucash/backend/xml/io-example-account.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/backend/xml/io-gncxml-gen.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/backend/xml/io-gncxml-v2.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/backend/xml/io-utils.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/backend/xml/gnc-account-xml-v2.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/backend/xml/gnc-budget-xml-v2.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/backend/xml/gnc-lot-xml-v2.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/backend/xml/gnc-recurrence-xml-v2.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/backend/xml/gnc-schedxaction-xml-v2.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/backend/xml/gnc-freqspec-xml-v2.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/backend/xml/gnc-transaction-xml-v2.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/backend/xml/gnc-commodity-xml-v2.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/backend/xml/gnc-book-xml-v2.cpp
  ${CMAKE_SOURCE_DIR}/libgnucash/backend/xml/gnc-pricedb-xml-v2.cpp
)

SET_LOCAL_DIST(test_backend_xml_DIST_local CMakeLists.txt grab-types.pl
  README test-date-converting.cpp test-dom-converters1.cpp
  test-dom-parser1.cpp test-file-stuff.cpp test-file-stuff.h test-kvp-frames.cpp
  test-load-backend.cpp test-load-example-account.cpp  test-load-xml2.cpp
  test-save-in-lang.cpp test-string-converters.cpp test-xml2-is-file.cpp
  test-xml-account.cpp test-real-data.sh.in test-xml-commodity.cpp
  test-xml-pricedb.cpp test-xml-transaction.cpp)
SET(test_backend_xml_DIST ${test_backend_xml_DIST_local} ${test_backend_xml_test_files_DIST} PARENT_SCOPE)

# The test test-dom-parser1.c is not run by 

# This line for autotools
CONFIGURE_FILE(test-real-data.sh.in test-real-data.sh)

ADD_XML_TEST(test-date-converting "${test_backend_xml_base_SOURCES};test-date-converting.cpp")
ADD_XML_TEST(test-dom-converters1 "${test_backend_xml_base_SOURCES};test-dom-converters1.cpp")
ADD_XML_TEST(test-kvp-frames      "${test_backend_xml_base_SOURCES};test-kvp-frames.cpp")
ADD_XML_TEST(test-load-backend  test-load-backend.cpp)
ADD_XML_TEST(test-load-xml2 test-load-xml2.cpp
  GNC_TEST_FILES=${CMAKE_CURRENT_SOURCE_DIR}/test-files/xml2
)
# Not run in autotools.
#ADD_XML_TEST(test-save-in-lang test-save-in-lang.cpp
#  GNC_TEST_FILES=${CMAKE_CURRENT_SOURCE_DIR}/test-files/xml2
#)

GNC_ADD_TEST_WITH_GUILE(test-load-example-account
  "${test_backend_xml_module_SOURCES};test-load-example-account.cpp"
  XML_TEST_INCLUDE_DIRS XML_TEST_LIBS
  GNC_ACCOUNT_PATH=${CMAKE_SOURCE_DIR}/data/accounts/C
)
TARGET_COMPILE_OPTIONS(test-load-example-account PRIVATE -DU_SHOW_CPLUSPLUS_API=0)
ADD_XML_TEST(test-string-converters "${test_backend_xml_base_SOURCES};test-string-converters.cpp")
ADD_XML_TEST(test-xml-account "${test_backend_xml_module_SOURCES};test-xml-account.cpp;test-file-stuff.cpp")
ADD_XML_TEST(test-xml-commodity "${test_backend_xml_module_SOURCES};test-xml-commodity.cpp;test-file-stuff.cpp")
ADD_XML_TEST(test-xml-pricedb "${test_backend_xml_module_SOURCES};test-xml-pricedb.cpp;test-file-stuff.cpp")
ADD_XML_TEST(test-xml-transaction "${test_backend_xml_module_SOURCES};test-xml-transaction.cpp;test-file-stuff.cpp")
ADD_XML_TEST(test-xml2-is-file "${test_backend_xml_module_SOURCES};test-xml2-is-file.cpp"
   GNC_TEST_FILES=${CMAKE_CURRENT_SOURCE_DIR}/test-files/xml2)

SET(CMAKE_COMMAND_TMP "")
IF (${CMAKE_VERSION} VERSION_GREATER 3.1)
  SET(CMAKE_COMMAND_TMP ${CMAKE_COMMAND} -E env)
ENDIF()

SET(test-real-data-env
  SRCDIR=${CMAKE_CURRENT_SOURCE_DIR}
  VERBOSE=yes
  TEST_PATH=${CMAKE_BINARY_DIR}/bin
)
ADD_TEST(NAME test-real-data
   COMMAND ${CMAKE_COMMAND_TMP}
    ${SHELL} ${CMAKE_CURRENT_SOURCE_DIR}/test-real-data.sh.in
)
SET_TESTS_PROPERTIES(test-real-data PROPERTIES ENVIRONMENT "${test-real-data-env}")
