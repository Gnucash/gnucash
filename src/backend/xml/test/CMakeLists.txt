# Common stuff
ADD_SUBDIRECTORY(test-files)

SET(XML_TEST_INCLUDE_DIRS
  ${CMAKE_SOURCE_DIR}/src/backend/xml
  ${CMAKE_SOURCE_DIR}/src/backend/xml/test
  ${CMAKE_SOURCE_DIR}/src/engine
  ${CMAKE_SOURCE_DIR}/src/engine/test-core
  ${CMAKE_SOURCE_DIR}/src/libqof/qof # for qof.h
  ${CMAKE_BINARY_DIR}/src # for config.h
  ${CMAKE_SOURCE_DIR}/src/test-core  # for unittest-support.h
  ${GLIB2_INCLUDE_DIRS}
  ${LIBXML2_INCLUDE_DIRS}
  ${ZLIB_INCLUDE_DIRS}
)


SET(XML_TEST_LIBS gncmod-engine gnc-qof gncmod-test-engine test-core ${LIBXML2_LDFLAGS} -lz)

FUNCTION(ADD_XML_TEST _TARGET _SOURCE_FILES)
  GNC_ADD_TEST(${_TARGET} "${_SOURCE_FILES}" XML_TEST_INCLUDE_DIRS XML_TEST_LIBS ${ARGN})
ENDFUNCTION()


################################

SET(test_backend_xml_base_SOURCES
  ${CMAKE_SOURCE_DIR}/src/backend/xml/sixtp-dom-parsers.c
  ${CMAKE_SOURCE_DIR}/src/backend/xml/sixtp-dom-generators.c
  ${CMAKE_SOURCE_DIR}/src/backend/xml/sixtp-utils.c
  ${CMAKE_SOURCE_DIR}/src/backend/xml/sixtp.c
  ${CMAKE_SOURCE_DIR}/src/backend/xml/sixtp-stack.c
  ${CMAKE_SOURCE_DIR}/src/backend/xml/sixtp-to-dom-parser.c
  ${CMAKE_SOURCE_DIR}/src/backend/xml/gnc-xml-helper.c
)

## the xml backend is now a GModule - this test does
## not load it as a module and cannot link to it
## and remain portable.

SET(test_backend_xml_module_SOURCES
  ${test_backend_xml_base_SOURCES}
  ${CMAKE_SOURCE_DIR}/src/backend/xml/io-example-account.c
  ${CMAKE_SOURCE_DIR}/src/backend/xml/io-gncxml-gen.c
  ${CMAKE_SOURCE_DIR}/src/backend/xml/io-gncxml-v2.c
  ${CMAKE_SOURCE_DIR}/src/backend/xml/io-utils.c
  ${CMAKE_SOURCE_DIR}/src/backend/xml/gnc-account-xml-v2.c
  ${CMAKE_SOURCE_DIR}/src/backend/xml/gnc-budget-xml-v2.c
  ${CMAKE_SOURCE_DIR}/src/backend/xml/gnc-lot-xml-v2.c
  ${CMAKE_SOURCE_DIR}/src/backend/xml/gnc-recurrence-xml-v2.c
  ${CMAKE_SOURCE_DIR}/src/backend/xml/gnc-schedxaction-xml-v2.c
  ${CMAKE_SOURCE_DIR}/src/backend/xml/gnc-freqspec-xml-v2.c
  ${CMAKE_SOURCE_DIR}/src/backend/xml/gnc-transaction-xml-v2.c
  ${CMAKE_SOURCE_DIR}/src/backend/xml/gnc-commodity-xml-v2.c
  ${CMAKE_SOURCE_DIR}/src/backend/xml/gnc-book-xml-v2.c
  ${CMAKE_SOURCE_DIR}/src/backend/xml/gnc-pricedb-xml-v2.c
)

SET_LOCAL_DIST(test_backend_xml_DIST_local CMakeLists.txt grab-types.pl Makefile.am README test-date-converting.c
        test-dom-converters1.c test-dom-parser1.c test-file-stuff.c test-file-stuff.h test-kvp-frames.c
        test-load-backend.c test-load-example-account.c  test-load-xml2.c test-save-in-lang.c test-string-converters.c
        test-xml2-is-file.c test-xml-account.c test-real-data.sh.in test-xml-commodity.c test-xml-pricedb.c
        test-xml-transaction.c)
SET(test_backend_xml_DIST ${test_backend_xml_DIST_local} ${test_backend_xml_test_files_DIST} PARENT_SCOPE)

# The test test-dom-parser1.c is not run by Makefile.am

# This line for autotools
CONFIGURE_FILE(test-real-data.sh.in test-real-data.sh)

ADD_XML_TEST(test-date-converting "${test_backend_xml_base_SOURCES};test-date-converting.c")
ADD_XML_TEST(test-dom-converters1 "${test_backend_xml_base_SOURCES};test-dom-converters1.c")
ADD_XML_TEST(test-kvp-frames      "${test_backend_xml_base_SOURCES};test-kvp-frames.c")
ADD_XML_TEST(test-load-backend  test-load-backend.c)
ADD_XML_TEST(test-load-xml2 test-load-xml2.c
  GNC_TEST_FILES=${CMAKE_CURRENT_SOURCE_DIR}/test-files/xml2
)
# Not run in autotools.
#ADD_XML_TEST(test-save-in-lang test-save-in-lang.c
#  GNC_TEST_FILES=${CMAKE_CURRENT_SOURCE_DIR}/test-files/xml2
#)

GNC_ADD_TEST_WITH_GUILE(test-load-example-account
  "${test_backend_xml_module_SOURCES};test-load-example-account.c"
  XML_TEST_INCLUDE_DIRS XML_TEST_LIBS
  GNC_ACCOUNT_PATH=${CMAKE_SOURCE_DIR}/accounts/C
)
ADD_XML_TEST(test-string-converters "${test_backend_xml_base_SOURCES};test-string-converters.c")
ADD_XML_TEST(test-xml-account "${test_backend_xml_module_SOURCES};test-xml-account.c;test-file-stuff.c")
ADD_XML_TEST(test-xml-commodity "${test_backend_xml_module_SOURCES};test-xml-commodity.c;test-file-stuff.c")
ADD_XML_TEST(test-xml-pricedb "${test_backend_xml_module_SOURCES};test-xml-pricedb.c;test-file-stuff.c")
ADD_XML_TEST(test-xml-transaction "${test_backend_xml_module_SOURCES};test-xml-transaction.c;test-file-stuff.c")
ADD_XML_TEST(test-xml2-is-file "${test_backend_xml_module_SOURCES};test-xml2-is-file.c"
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
