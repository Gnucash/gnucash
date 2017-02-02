CONFIGURE_FILE(test-version.in test-version)

ADD_TEST(NAME test-version
         COMMAND ${CMAKE_BINARY_DIR}/bin/gnucash --version)

SET_DIST_LIST(test_bin_DIST CMakeLists.txt Makefile.am test-version.in)