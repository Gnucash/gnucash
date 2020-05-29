set(engine_mocks_SOURCES
    gmock-Account.cpp
    gmock-Account.h
    gmock-gobject.h
    gmock-qofbook.cpp
    gmock-qofbook.h
    gmock-qofinstance.cpp
    gmock-qofquery.cpp
    gmock-qofquery.h
    gmock-Split.cpp
    gmock-Split.h
    gmock-Transaction.cpp
    gmock-Transaction.h
)


set_dist_list(engine_mocks_DIST
    CMakeLists.txt
    ${engine_mocks_SOURCES})

