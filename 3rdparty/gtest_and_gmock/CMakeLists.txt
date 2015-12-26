# Download and unpack gtest at configure time
configure_file(gtest_and_gmock_download.cmake ${CMAKE_BINARY_DIR}/gtest_and_gmock_download/CMakeLists.txt)
execute_process(COMMAND ${CMAKE_COMMAND} -G "${CMAKE_GENERATOR}" .
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/gtest_and_gmock_download )
execute_process(COMMAND ${CMAKE_COMMAND} --build .
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/gtest_and_gmock_download )

# Add gtest directly to our build
# WARNING: Only add googlemock, googletest will be build as dependency.
# If you add the two directories build will fail
add_subdirectory(${CMAKE_BINARY_DIR}/gmock-src/googlemock
    ${CMAKE_BINARY_DIR}/gmock-build
    EXCLUDE_FROM_ALL )

# We need both gtest and gmock files to write our tests
target_include_directories(gtest
    INTERFACE
    "${CMAKE_BINARY_DIR}/gmock-src/googletest/include"
    )

target_include_directories(gmock
    INTERFACE
    "${CMAKE_BINARY_DIR}/gmock-src/googlemock/include"
    )
