SET(gnc-qof-test-SOURCES 
    gtest-gnc-int128.cpp
    gtest-gnc-datetime.cpp
    gtest-gnc-timezone.cpp
    gtest-kvp-frame.cpp
    gtest-kvp-value.cpp)

ADD_EXECUTABLE(gnc-qof-test ${gnc-qof-test-SOURCES})
TARGET_LINK_LIBRARIES(gnc-qof-test gnc-qof gtest_main)
