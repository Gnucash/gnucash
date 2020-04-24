

set(libgnc_test_engine_SOURCES
    test-engine-stuff.cpp
)

add_library(gnc-test-engine STATIC ${libgnc_test_engine_SOURCES})

target_include_directories(gnc-test-engine PRIVATE
  ${GMODULE_INCLUDE_DIRS}
  ${CMAKE_BINARY_DIR}/common # for config.h
  ${CMAKE_SOURCE_DIR}/common
  ${CMAKE_SOURCE_DIR}/libgnucash/engine
  ${CMAKE_SOURCE_DIR}/common/test-core
)

set_dist_list(engine_test_core_DIST CMakeLists.txt ${libgnc_test_engine_SOURCES}
        test-engine-stuff.h test-engine-strings.h)
