

set(libgncmod_test_engine_SOURCES
  gncmod-test-engine.c test-engine-stuff.cpp
)

add_library(gncmod-test-engine STATIC ${libgncmod_test_engine_SOURCES})

target_include_directories(gncmod-test-engine PRIVATE
  ${GMODULE_INCLUDE_DIRS}
  ${CMAKE_SOURCE_DIR}/libgnucash/gnc-module
  ${CMAKE_BINARY_DIR}/common # for config.h
  ${CMAKE_SOURCE_DIR}/common
  ${CMAKE_SOURCE_DIR}/libgnucash/engine
  ${CMAKE_SOURCE_DIR}/common/test-core
)

set_dist_list(engine_test_core_DIST CMakeLists.txt ${libgncmod_test_engine_SOURCES}
        test-engine-stuff.h test-engine-strings.h)
