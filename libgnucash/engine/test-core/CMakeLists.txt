

SET(libgncmod_test_engine_SOURCES
  gncmod-test-engine.c test-engine-stuff.cpp
)

ADD_LIBRARY(gncmod-test-engine STATIC ${libgncmod_test_engine_SOURCES})

TARGET_INCLUDE_DIRECTORIES(gncmod-test-engine PRIVATE
  ${CMAKE_PREFIX_PATH}/include
  ${GMODULE_INCLUDE_DIRS}
  ${CMAKE_SOURCE_DIR}/libgnucash/gnc-module
  ${CMAKE_BINARY_DIR}/common # for config.h
  ${CMAKE_SOURCE_DIR}/common
  ${CMAKE_SOURCE_DIR}/libgnucash/engine
  ${CMAKE_SOURCE_DIR}/common/test-core
)

SET_DIST_LIST(engine_test_core_DIST CMakeLists.txt ${libgncmod_test_engine_SOURCES}
        test-engine-stuff.h test-engine-strings.h)
