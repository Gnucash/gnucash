set(test_core_SOURCES
  test-stuff.c
  unittest-support.c
)

set(test_core_noinst_HEADERS
  test-stuff.h
  unittest-support.h
)

include_directories(
  ${CMAKE_BINARY_DIR}/common
  ${CMAKE_SOURCE_DIR}/common
  ${CMAKE_SOURCE_DIR}/libgnucash/engine
  ${CMAKE_SOURCE_DIR}/common/test-core

  ${GUILE_INCLUDE_DIRS}
)

set_dist_list(test_core_DIST ${test_core_SOURCES} ${test_core_noinst_HEADERS} CMakeLists.txt
        unittest-support.i unittest-support.scm)

add_library(test-core STATIC ${test_core_SOURCES} ${test_core_noinst_HEADERS})
target_link_libraries(test-core gnc-engine PkgConfig::GLIB2)
if (UNIX)
  target_compile_options(test-core PRIVATE -fPIC)
endif()

# Command to generate the swig-unittest-support-guile.c wrapper file
gnc_add_swig_guile_command (swig-unittest-support-guile-c
    SWIG_UNITTEST_SUPPORT_GUILE_C swig-unittest-support-guile.c
    ${CMAKE_CURRENT_SOURCE_DIR}/unittest-support.i "" ${test_core_HEADERS}
)

# Command to generate the swig-unittest-support-python.c wrapper file
gnc_add_swig_python_command (swig-unittest-support-python
    SWIG_UNITTEST_SUPPORT_PYTHON_C SWIG_UNITTEST_SUPPORT_PYTHON_PY
    swig-unittest-support-python.c unittest_support.py
    ${CMAKE_CURRENT_SOURCE_DIR}/unittest-support.i ""
)

add_library(test-core-guile ${SWIG_UNITTEST_SUPPORT_GUILE_C})
target_link_libraries(test-core-guile test-core ${GUILE_LDFLAGS} PkgConfig::GLIB2)
add_dependencies (test-core-guile swig-unittest-support-guile-c )

if (WITH_PYTHON)
  add_library(unittest_support MODULE ${SWIG_UNITTEST_SUPPORT_PYTHON_C})
  target_link_libraries(unittest_support test-core ${Python3_LIBRARIES})
  target_include_directories(unittest_support PRIVATE ${Python3_INCLUDE_DIRS})
  set_target_properties(unittest_support PROPERTIES PREFIX "_")
  if (HAVE_STRINGOP_TRUNCATION)
    target_compile_options(unittest_support PRIVATE -Wno-error=stringop-truncation)
  endif()
endif()

set(test_core_SCHEME unittest-support.scm)

set(GUILE_OUTPUT_DIR   tests)
set(GUILE_DEPENDS      test-core-guile)


gnc_add_scheme_test_targets(scm-test-core
    SOURCES ${test_core_SCHEME}
    OUTPUT_DIR ${GUILE_OUTPUT_DIR}
    DEPENDS "${GUILE_DEPENDS}")

add_dependencies(check scm-test-core)


if (GTEST_SRC_DIR)
  # in contrast to GoogleTest build system libraries libgtest.a and libgtest_main.a
  # are combined to one library libtest.a here
  add_library(gtest STATIC  ${lib_gtest_SOURCES})
  if(APPLE)
    target_compile_options(gtest PRIVATE -Wno-missing-prototypes)
  else()
    target_compile_options(gtest PRIVATE -Wno-missing-declarations)
  endif()
  target_link_libraries(gtest Threads::Threads)
  target_include_directories(gtest PUBLIC ${GTEST_INCLUDE_DIR} ${GTEST_SRC_DIR})
else()
  add_library(gtest UNKNOWN IMPORTED GLOBAL)
  set_target_properties(gtest PROPERTIES
    IMPORTED_LOCATION ${GTEST_SHARED_LIB}
    INTERFACE_INCLUDE_DIRECTORIES ${GTEST_INCLUDE_DIR}
    INTERFACE_LINK_LIBRARIES ${GTEST_MAIN_LIB}
  )
endif()
if (GMOCK_SRC_DIR)
  set (lib_gmock_SOURCES ${GMOCK_SRC})
  add_library(gmock STATIC  ${lib_gmock_SOURCES})
  if (APPLE)
    target_compile_options(gmock PRIVATE -Wno-missing-prototypes)
  else()
    target_compile_options(gmock PRIVATE -Wno-missing-declarations)
  endif()
  target_include_directories(gmock PUBLIC
    ${GTEST_INCLUDE_DIR} ${GTEST_SRC_DIR}
    ${GMOCK_INCLUDE_DIR} ${GMOCK_SRC_DIR})
endif()

add_executable(jenny  ${CMAKE_SOURCE_DIR}/borrowed/jenny/jenny.c)
