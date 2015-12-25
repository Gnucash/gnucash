cmake_minimum_required(VERSION 3.2)

project(gtest_and_gmock_download LANGUAGES NONE)

include(ExternalProject)

ExternalProject_Add(googletest
    GIT_REPOSITORY git@github.com:google/googletest.git
    SOURCE_DIR "${CMAKE_BINARY_DIR}/gmock-src"
    BINARY_DIR "${CMAKE_BINARY_DIR}/gmock-build"
    CONFIGURE_COMMAND ""
    BUILD_COMMAND     ""
    INSTALL_COMMAND   ""
    TEST_COMMAND      ""
    )
