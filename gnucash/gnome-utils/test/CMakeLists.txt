
set(GNOME_UTILS_GUI_TEST_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common
  ${CMAKE_BINARY_DIR}/gnucash/gnome-utils
  ${CMAKE_SOURCE_DIR}/gnucash/gnome-utils
  ${CMAKE_SOURCE_DIR}/libgnucash/core-utils
  ${CMAKE_SOURCE_DIR}/libgnucash/engine
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

gnc_add_test (gtest-dialog-object-references-response
  gtest-dialog-object-references-response.cpp
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)

gnc_add_test (gtest-dialog-date-range-response
  gtest-dialog-date-range-response.cpp
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)
set_tests_properties(gtest-dialog-date-range-response PROPERTIES TIMEOUT 15)

gnc_add_test (gtest-gui-query-response
  gtest-gui-query-response.cpp
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)
set_tests_properties(gtest-gui-query-response PROPERTIES TIMEOUT 30)

gnc_add_test (gtest-input-lifetime-response
  gtest-input-lifetime-response.cpp
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)
set_tests_properties(gtest-input-lifetime-response PROPERTIES TIMEOUT 30)

gnc_add_test (gtest-dialog-account-cascade-response
  gtest-dialog-account-cascade-response.cpp
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)
set_tests_properties(gtest-dialog-account-cascade-response PROPERTIES TIMEOUT 30)

gnc_add_test (gtest-doclink-path-head-response
  gtest-doclink-path-head-response.cpp
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)
set_tests_properties(gtest-doclink-path-head-response PROPERTIES TIMEOUT 30)

gnc_add_test (gtest-gnc-autosave-response
  gtest-gnc-autosave-response.cpp
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)
set_tests_properties(gtest-gnc-autosave-response PROPERTIES TIMEOUT 30)

gnc_add_test (gtest-preferences-separator-response
  gtest-preferences-separator-response.cpp
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)
set_tests_properties(gtest-preferences-separator-response PROPERTIES TIMEOUT 30)

gnc_add_test (gtest-xml-encoding-assistant
  gtest-xml-encoding-assistant.cpp
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)
set_tests_properties(gtest-xml-encoding-assistant PROPERTIES TIMEOUT 30)

gnc_add_test (gtest-dialog-commodity-response
  gtest-dialog-commodity-response.cpp
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)
set_tests_properties(gtest-dialog-commodity-response PROPERTIES TIMEOUT 30)

gnc_add_test (gtest-account-children-response
  gtest-account-children-response.cpp
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)
set_tests_properties(gtest-account-children-response PROPERTIES TIMEOUT 30)

# Use the actual main-window resource without depending on generation in the
# application target, which brings unrelated report/HTML libraries into this test.
gnc_generate_gresources(BASE test-main-window-resources
  RESOURCE_FILES ${CMAKE_SOURCE_DIR}/gnucash/ui/gnc-main-window.ui)
set(MAIN_WINDOW_CLOSE_TEST_SOURCES
  gtest-main-window-close-response.cpp
  ${CMAKE_CURRENT_BINARY_DIR}/test-main-window-resources.c)
gnc_add_test(gtest-main-window-close-response
  "${MAIN_WINDOW_CLOSE_TEST_SOURCES}"
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS
  "GSETTINGS_BACKEND=memory")
set_tests_properties(gtest-main-window-close-response PROPERTIES TIMEOUT 30)

gnc_add_test_with_guile (gtest-tax-table-create-response
  gtest-tax-table-create-response.cpp
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)
add_dependencies(gtest-tax-table-create-response scm-expressions)
set_tests_properties(gtest-tax-table-create-response PROPERTIES TIMEOUT 30)

foreach(preference_test IN ITEMS gtest-gui-query-response gtest-input-lifetime-response
    gtest-account-children-response gtest-main-window-close-response
    gtest-tax-table-create-response)
  set_property(TEST ${preference_test} APPEND PROPERTY ENVIRONMENT
    "GSETTINGS_SCHEMA_DIR=${SCHEMADIR_BUILD}" "GSETTINGS_BACKEND=memory")
  if(COMPILE_GSCHEMAS)
    add_dependencies(${preference_test} compiled-schemas)
  endif()
endforeach()

gnc_add_test (gtest-file-error-response
  gtest-file-error-response.cpp
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)
set_tests_properties(gtest-file-error-response PROPERTIES TIMEOUT 30)

gnc_add_test (gtest-file-chooser-async-response
  gtest-file-chooser-async-response.cpp
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)
set_tests_properties(gtest-file-chooser-async-response PROPERTIES TIMEOUT 30)

gnc_add_test(gtest-file-save-response gtest-file-save-response.cpp
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)
set_tests_properties(gtest-file-save-response PROPERTIES TIMEOUT 30)
gnc_add_test(gtest-file-open-response gtest-file-open-response.cpp
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)
set_tests_properties(gtest-file-open-response PROPERTIES TIMEOUT 30)
set_property(TEST gtest-file-open-response APPEND PROPERTY ENVIRONMENT
  "GSETTINGS_SCHEMA_DIR=${SCHEMADIR_BUILD}" "GSETTINGS_BACKEND=memory")
if(COMPILE_GSCHEMAS)
  add_dependencies(gtest-file-open-response compiled-schemas)
endif()
set_property(TEST gtest-file-save-response APPEND PROPERTY ENVIRONMENT
  "GSETTINGS_SCHEMA_DIR=${SCHEMADIR_BUILD}" "GSETTINGS_BACKEND=memory")
if(COMPILE_GSCHEMAS)
  add_dependencies(gtest-file-save-response compiled-schemas)
endif()

gnc_add_test (gtest-general-select-response
  gtest-general-select-response.cpp
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)
set_tests_properties(gtest-general-select-response PROPERTIES TIMEOUT 30)

gnc_add_test(gtest-budget-model-identity gtest-budget-model-identity.cpp
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)
set_tests_properties(gtest-budget-model-identity PROPERTIES TIMEOUT 30)

set_dist_list(test_gnome_utils_DIST
  CMakeLists.txt
  test-load-gnome-utils-module.scm
  gtest-dialog-object-references-response.cpp
  gtest-dialog-date-range-response.cpp
  gtest-gui-query-response.cpp
  gtest-input-lifetime-response.cpp
  gtest-dialog-account-cascade-response.cpp
  gtest-doclink-path-head-response.cpp
  gtest-gnc-autosave-response.cpp
  gtest-preferences-separator-response.cpp
  gtest-xml-encoding-assistant.cpp
  gtest-dialog-commodity-response.cpp
  gtest-account-children-response.cpp
  gtest-main-window-close-response.cpp
  gtest-tax-table-create-response.cpp
  gtest-file-error-response.cpp
  gtest-file-chooser-async-response.cpp
  gtest-file-save-response.cpp
  gtest-file-open-response.cpp
  gtest-general-select-response.cpp
  gtest-budget-model-identity.cpp)
