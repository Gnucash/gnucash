set(MODULEPATH ${CMAKE_SOURCE_DIR}/libgnucash/core-utils)

SET(CORE_UTILS_TEST_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common # for config.h
  ${MODULEPATH}
  ${CMAKE_SOURCE_DIR}/common/test-core
  ${GLIB2_INCLUDE_DIRS}
  ${GTK_MAC_INCLUDE_DIRS}
)
SET(CORE_UTILS_TEST_LIBS gnc-core-utils test-core)
IF (MAC_INTEGRATION)
  LIST(APPEND CORE_UTILS_TEST_LIBS ${OSX_EXTRA_LIBRARIES})
ENDIF()

MACRO(ADD_CORE_UTILS_TEST _TARGET _SOURCE_FILES)
  GNC_ADD_TEST(${_TARGET} "${_SOURCE_FILES}" CORE_UTILS_TEST_INCLUDE_DIRS CORE_UTILS_TEST_LIBS)
ENDMACRO()

ADD_CORE_UTILS_TEST(test-gnc-glib-utils test-gnc-glib-utils.c)
ADD_CORE_UTILS_TEST(test-resolve-file-path test-resolve-file-path.c)
ADD_CORE_UTILS_TEST(test-userdata-dir test-userdata-dir.c)
IF (NOT MAC_INTEGRATION AND NOT WIN32)
  ADD_CORE_UTILS_TEST(test-userdata-dir-invalid-home test-userdata-dir-invalid-home.c)
ENDIF()
IF (MAC_INTEGRATION)
  TARGET_COMPILE_OPTIONS(test-userdata-dir PRIVATE ${OSX_EXTRA_COMPILE_FLAGS})
  TARGET_COMPILE_DEFINITIONS(test-userdata-dir PRIVATE ${GTK_MAC_CFLAGS_OTHER})
ENDIF(MAC_INTEGRATION)

set(gtest_core_utils_INCLUDES
  ${MODULEPATH}
  ${CMAKE_BINARY_DIR}/common # for config.h
  ${CMAKE_SOURCE_DIR}/common # for platform.h
  ${CMAKE_BINARY_DIR}/libgnucash/core-utils # for gncla-dir.h
  ${GLIB2_INCLUDE_DIRS}
  ${GTEST_INCLUDE_DIR}
  )

SET(gtest_core_utils_LIBS
  ${GLIB2_LDFLAGS}
  ${Boost_LIBRARIES}
  ${GTHREAD_LDFLAGS}
  ${GTEST_LIB})

set(test_gnc_path_util_SOURCES
  ${MODULEPATH}/gnc-path.c
  ${MODULEPATH}/binreloc.c
  ${MODULEPATH}/gnc-filepath-utils.cpp
  gtest-path-utilities.cpp
  ${GTEST_SRC})

gnc_add_test(test-gnc-path-util "${test_gnc_path_util_SOURCES}"
  gtest_core_utils_INCLUDES gtest_core_utils_LIBS "GNC_UNINSTALLED=yes")

SET_DIST_LIST(test_core_utils_DIST CMakeLists.txt
  test-gnc-glib-utils.c test-resolve-file-path.c test-userdata-dir.c
  test-userdata-dir-invalid-home.c gtest-path-utilities.cpp)
