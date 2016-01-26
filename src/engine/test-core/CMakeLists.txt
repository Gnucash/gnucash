

SET(libgncmod_test_engine_SOURCES
  gncmod-test-engine.c test-engine-stuff.c
)

ADD_LIBRARY(gncmod-test-engine STATIC ${libgncmod_test_engine_SOURCES})

TARGET_INCLUDE_DIRECTORIES(gncmod-test-engine PRIVATE
  ${GMODULE_INCLUDE_DIRS}
  ${CMAKE_SOURCE_DIR}/src/gnc-module
  ${CMAKE_SOURCE_DIR}/src/libqof/qof
  ${CMAKE_BINARY_DIR}/src # for config.h
  ${CMAKE_SOURCE_DIR}/src
  ${CMAKE_SOURCE_DIR}/src/engine
  ${CMAKE_SOURCE_DIR}/src/test-core
)