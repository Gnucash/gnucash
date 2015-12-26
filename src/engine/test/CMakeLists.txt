SET(gnc-engine-test-SOURCES gtest-import-map.cpp)

ADD_EXECUTABLE(gnc-engine-test ${gnc-engine-test-SOURCES})
TARGET_LINK_LIBRARIES(gnc-engine-test gncmod-engine gtest_main)
