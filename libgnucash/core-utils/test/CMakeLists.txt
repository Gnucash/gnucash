set(MODULEPATH ${CMAKE_SOURCE_DIR}/libgnucash/core-utils)

set(CORE_UTILS_TEST_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common # for config.h
  ${MODULEPATH}
  ${CMAKE_SOURCE_DIR}/common/test-core
)
set(CORE_UTILS_TEST_LIBS gnc-core-utils test-core)
if (MAC_INTEGRATION)
  list(APPEND CORE_UTILS_TEST_LIBS ${OSX_EXTRA_LIBRARIES})
endif()

macro(add_core_utils_test _TARGET _SOURCE_FILES)
  gnc_add_test(${_TARGET} "${_SOURCE_FILES}" CORE_UTILS_TEST_INCLUDE_DIRS CORE_UTILS_TEST_LIBS)
endmacro()

set(gtest_string_utils_INCLUDES
  ${CMAKE_BINARY_DIR}/common # for config.h
  ${MODULEPATH}
  ${CMAKE_SOURCE_DIR}/common/test-core)

set(gtest_string_utils_LIBS gnc-core-utils test-core gtest)

gnc_add_test(test-gnc-string-utils gtest-gnc-string-utils.cpp
  gtest_string_utils_INCLUDES gtest_string_utils_LIBS)
add_core_utils_test(test-resolve-file-path test-resolve-file-path.c)
add_core_utils_test(test-userdata-dir test-userdata-dir.c)
if (NOT MAC_INTEGRATION AND NOT WIN32)
  add_core_utils_test(test-userdata-dir-invalid-home test-userdata-dir-invalid-home.c)
endif()
if (MAC_INTEGRATION)
  target_compile_options(test-userdata-dir PRIVATE ${OSX_EXTRA_COMPILE_FLAGS})
endif()

set(gtest_core_utils_INCLUDES
  ${MODULEPATH}
  ${CMAKE_BINARY_DIR}/common # for config.h
  ${CMAKE_SOURCE_DIR}/common # for platform.h
  ${CMAKE_BINARY_DIR}/libgnucash/core-utils # for gncla-dir.h
  )

set(gtest_core_utils_LIBS
  PkgConfig::GLIB2
  ${Boost_LIBRARIES}
  ${GTHREAD_LDFLAGS}
  ctre
  gtest)
if (MAC_INTEGRATION)
  list(APPEND gtest_core_utils_LIBS ${OSX_EXTRA_LIBRARIES})
endif()

set(test_gnc_path_util_SOURCES
  ${MODULEPATH}/gnc-path.c
  ${MODULEPATH}/binreloc.c
  ${MODULEPATH}/gnc-filepath-utils.cpp
  ${MODULEPATH}/gnc-locale-utils.cpp
  gtest-path-utilities.cpp)

gnc_add_test(test-gnc-path-util "${test_gnc_path_util_SOURCES}"
  gtest_core_utils_INCLUDES gtest_core_utils_LIBS "GNC_UNINSTALLED=yes")
if (MAC_INTEGRATION)
  target_compile_options(test-gnc-path-util PRIVATE ${OSX_EXTRA_COMPILE_FLAGS})
endif()

set(gtest_icu_locale_INCLUDES
  ${CMAKE_BINARY_DIR}/common
  ${MODULEPATH})

set(gtest_icu_locale_LIBS
  ${Boost_LIBRARIES}
  ICU::uc
  ICU::i18n
  PkgConfig::GLIB2
  gtest)

gnc_add_test(test-gnc-unicode
  "${MODULEPATH}/gnc-unicode.cpp;${MODULEPATH}/gnc-locale-utils.c;gtest-gnc-unicode.cpp"
  gtest_icu_locale_INCLUDES
  gtest_icu_locale_LIBS)

set_dist_list(test_core_utils_DIST CMakeLists.txt
  gtest-gnc-string-utils.cpp test-resolve-file-path.c test-userdata-dir.c
  test-userdata-dir-invalid-home.c gtest-gnc-unicode.cpp gtest-path-utilities.cpp)
