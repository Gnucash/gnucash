# Common stuff
add_subdirectory(test-files)

set(XML_TEST_INCLUDE_DIRS
  ${CMAKE_SOURCE_DIR}/libgnucash/backend/xml
  ${CMAKE_SOURCE_DIR}/libgnucash/backend/xml/test
  ${CMAKE_SOURCE_DIR}/libgnucash/engine
  ${CMAKE_SOURCE_DIR}/libgnucash/engine/test-core
  ${CMAKE_BINARY_DIR}/common # for config.h
  ${CMAKE_SOURCE_DIR}/common/test-core  # for unittest-support.h
  ${LIBXML2_INCLUDE_DIRS}
  ${ZLIB_INCLUDE_DIRS}
)


set(XML_TEST_LIBS gnc-backend-xml-utils gnc-engine gnc-test-engine test-core ${LIBXML2_LDFLAGS} -lz)
set(XML_GTEST_LIBS ${XML_TEST_LIBS} gtest)

function(add_xml_test _TARGET _SOURCE_FILES)
  gnc_add_test(${_TARGET} "${_SOURCE_FILES}" XML_TEST_INCLUDE_DIRS XML_TEST_LIBS ${ARGN})
  target_compile_options(${_TARGET} PRIVATE -DU_SHOW_CPLUSPLUS_API=0 -DG_LOG_DOMAIN=\"gnc.backend.xml\")
endfunction()

function(add_xml_gtest _TARGET _SOURCE_FILES)
  gnc_add_test(${_TARGET} "${_SOURCE_FILES}" XML_TEST_INCLUDE_DIRS XML_GTEST_LIBS ${ARGN})
  target_compile_options(${_TARGET} PRIVATE -DU_SHOW_CPLUSPLUS_API=0 -DG_LOG_DOMAIN=\"gnc.backend.xml\")
endfunction()

set_local_dist(test_backend_xml_DIST_local
  CMakeLists.txt
  grab-types.pl
  README
  test-dom-converters1.cpp
  test-dom-parser1.cpp
  test-file-stuff.cpp
  test-file-stuff.h
  test-kvp-frames.cpp
  test-load-backend.cpp
  test-load-example-account.cpp
  gtest-load-save-files.cpp
  test-load-xml2.cpp
  test-real-data.sh
  test-save-in-lang.cpp
  test-string-converters.cpp
  test-xml2-is-file.cpp
  test-xml-account.cpp
  test-xml-commodity.cpp
  test-xml-pricedb.cpp
  test-xml-transaction.cpp
)
set(test_backend_xml_DIST ${test_backend_xml_DIST_local} ${test_backend_xml_test_files_DIST} PARENT_SCOPE)

add_xml_test(test-dom-converters1 "test-dom-converters1.cpp")
add_xml_test(test-kvp-frames      "test-kvp-frames.cpp")
add_xml_test(test-load-backend  test-load-backend.cpp)
add_xml_test(test-load-xml2 test-load-xml2.cpp
  GNC_TEST_FILES=${CMAKE_CURRENT_SOURCE_DIR}/test-files/xml2
)
# FIXME Why is this test not run/running ?
#add_xml_test(test-save-in-lang test-save-in-lang.cpp
#  GNC_TEST_FILES=${CMAKE_CURRENT_SOURCE_DIR}/test-files/xml2
#)

add_xml_test(test-load-example-account
  "test-load-example-account.cpp"
  GNC_ACCOUNT_PATH=${CMAKE_SOURCE_DIR}/data/accounts/C
)
target_compile_options(test-load-example-account PRIVATE -DU_SHOW_CPLUSPLUS_API=0)
add_xml_gtest(test-load-save-files gtest-load-save-files.cpp
  GNC_TEST_FILES=${CMAKE_CURRENT_SOURCE_DIR}/test-files/load-save
)
add_xml_test(test-string-converters "test-string-converters.cpp")
add_xml_test(test-xml-account "test-xml-account.cpp;test-file-stuff.cpp")
add_xml_test(test-xml-commodity "test-xml-commodity.cpp;test-file-stuff.cpp")
add_xml_test(test-xml-pricedb "test-xml-pricedb.cpp;test-file-stuff.cpp")
add_xml_test(test-xml-transaction "test-xml-transaction.cpp;test-file-stuff.cpp")
add_xml_test(test-xml2-is-file "test-xml2-is-file.cpp"
   GNC_TEST_FILES=${CMAKE_CURRENT_SOURCE_DIR}/test-files/xml2)

set(test-real-data-env
  SRCDIR=${CMAKE_CURRENT_SOURCE_DIR}
  VERBOSE=yes
  TEST_PATH=${CMAKE_BINARY_DIR}/bin
)
add_test(NAME test-real-data
   COMMAND ${SHELL} ${CMAKE_CURRENT_SOURCE_DIR}/test-real-data.sh
   CONFIGURATIONS Debug;Release
)
set_tests_properties(test-real-data PROPERTIES ENVIRONMENT "${test-real-data-env}")
