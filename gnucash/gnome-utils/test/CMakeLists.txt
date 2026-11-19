
set(GNOME_UTILS_GUI_TEST_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common
  ${CMAKE_SOURCE_DIR}/gnucash/gnome-utils
  ${CMAKE_SOURCE_DIR}/libgnucash/engine
  ${CMAKE_SOURCE_DIR}/libgnucash/app-utils
  ${GUILE_INCLUDE_DIRS}
)
set(GNOME_UTILS_GUI_TEST_LIBS
  test-core
  gnc-gnome-utils
  gtest
)

set(GUILE_DEPENDS
  scm-gnome-utils
  test-core
  gnc-test-engine
  gnc-app-utils
  gnc-core-utils
  gnc-engine
)

gnc_add_scheme_test_targets(scm-test-load-gnome-utils-module
    SOURCES "test-load-gnome-utils-module.scm"
    OUTPUT_DIR "tests"
    DEPENDS "${GUILE_DEPENDS}")

gnc_add_scheme_tests(test-load-gnome-utils-module.scm)


set(GNOME_UTILS_GUI_TESTS
  test-gnc-allocation-categories
  test-gnc-option-gtk-ui
  test-gnc-tree-view-account
  test-dialog-options
)

get_guile_env()

foreach(gui_test IN LISTS GNOME_UTILS_GUI_TESTS)
  gnc_add_test(${gui_test} "${gui_test}.cpp"
    GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS
    "${GUILE_ENV}"
    "GSETTINGS_BACKEND=memory" "G_DEBUG=fatal-warnings" "NO_AT_BRIDGE=1" "LC_ALL=C"
    "GSETTINGS_SCHEMA_DIR=${CMAKE_BINARY_DIR}/share/glib-2.0/schemas")
  set_tests_properties(${gui_test} PROPERTIES SKIP_RETURN_CODE 77)
endforeach()

set_dist_list(test_gnome_utils_DIST CMakeLists.txt test-load-gnome-utils-module.scm
  test-option-ui.hpp
  test-gnc-allocation-categories.cpp
  test-gnc-option-gtk-ui.cpp
  test-gnc-tree-view-account.cpp
  test-dialog-options.cpp)
