set(MODULEPATH ${CMAKE_SOURCE_DIR}/gnucash/gnome/)

set(GNOME_TEST_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common # for config.h
  ${CMAKE_SOURCE_DIR}/common/test-core
  ${CMAKE_SOURCE_DIR}/gnucash/gnome/
  ${CMAKE_SOURCE_DIR}/gnucash/gnome-utils/
  ${GUILE_INCLUDE_DIRS}
)

set(GNOME_TEST_LIBS
        gnc-gnome
        gnc-test-engine
        test-core
        ${GUILE_LDFLAGS})

macro(add_gnome_test _TARGET _SOURCE_FILES)
  gnc_add_test(${_TARGET} "${_SOURCE_FILES}" GNOME_TEST_INCLUDE_DIRS GNOME_TEST_LIBS)
endmacro()

gnc_add_test_with_guile(test-invoice-report-buitin-default test-invoice-report-buitin-default.cpp
  GNOME_TEST_INCLUDE_DIRS GNOME_TEST_LIBS
)

set(GUILE_DEPENDS
  scm-core-utils
)

set_dist_list(test_app_utils_DIST
  CMakeLists.txt
  test-invoice-buitin-default.cpp
  ${test_gnome_scheme_SOURCES}
  ${test_gnome_SOURCES}
)
