
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

gnc_add_test (test-dialog-object-references-response
  test-dialog-object-references-response.cpp
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)

gnc_add_test (test-dialog-date-range-response
  test-dialog-date-range-response.cpp
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)
set_tests_properties(test-dialog-date-range-response PROPERTIES TIMEOUT 15)

gnc_add_test (test-gui-query-response
  test-gui-query-response.cpp
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)
set_tests_properties(test-gui-query-response PROPERTIES TIMEOUT 30)

gnc_add_test (test-input-lifetime-response
  test-input-lifetime-response.cpp
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)
set_tests_properties(test-input-lifetime-response PROPERTIES TIMEOUT 30)

gnc_add_test (test-dialog-account-cascade-response
  test-dialog-account-cascade-response.cpp
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)
set_tests_properties(test-dialog-account-cascade-response PROPERTIES TIMEOUT 30)

gnc_add_test (test-doclink-path-head-response
  test-doclink-path-head-response.cpp
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)
set_tests_properties(test-doclink-path-head-response PROPERTIES TIMEOUT 30)

gnc_add_test (test-gnc-autosave-response
  test-gnc-autosave-response.cpp
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)
set_tests_properties(test-gnc-autosave-response PROPERTIES TIMEOUT 30)

gnc_add_test (test-preferences-separator-response
  test-preferences-separator-response.cpp
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)
set_tests_properties(test-preferences-separator-response PROPERTIES TIMEOUT 30)

gnc_add_test (test-xml-encoding-assistant
  test-xml-encoding-assistant.cpp
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)
set_tests_properties(test-xml-encoding-assistant PROPERTIES TIMEOUT 30)

gnc_add_test (test-dialog-commodity-response
  test-dialog-commodity-response.cpp
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)
set_tests_properties(test-dialog-commodity-response PROPERTIES TIMEOUT 30)

gnc_add_test (test-account-children-response
  test-account-children-response.cpp
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)
set_tests_properties(test-account-children-response PROPERTIES TIMEOUT 30)

# Use the actual main-window resource without depending on generation in the
# application target, which brings unrelated report/HTML libraries into this test.
gnc_generate_gresources(BASE test-main-window-resources
  RESOURCE_FILES ${CMAKE_SOURCE_DIR}/gnucash/ui/gnc-main-window.ui)
set(MAIN_WINDOW_CLOSE_TEST_SOURCES
  test-main-window-close-response.cpp
  ${CMAKE_CURRENT_BINARY_DIR}/test-main-window-resources.c)
gnc_add_test(test-main-window-close-response
  "${MAIN_WINDOW_CLOSE_TEST_SOURCES}"
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS
  "GSETTINGS_BACKEND=memory")
set_tests_properties(test-main-window-close-response PROPERTIES TIMEOUT 30)

gnc_add_test_with_guile (test-tax-table-create-response
  test-tax-table-create-response.cpp
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)
add_dependencies(test-tax-table-create-response scm-expressions)
set_tests_properties(test-tax-table-create-response PROPERTIES TIMEOUT 30)

foreach(preference_test IN ITEMS test-gui-query-response test-input-lifetime-response
    test-account-children-response test-main-window-close-response
    test-tax-table-create-response)
  set_property(TEST ${preference_test} APPEND PROPERTY ENVIRONMENT
    "GSETTINGS_SCHEMA_DIR=${SCHEMADIR_BUILD}" "GSETTINGS_BACKEND=memory")
  if(COMPILE_GSCHEMAS)
    add_dependencies(${preference_test} compiled-schemas)
  endif()
endforeach()

gnc_add_test (test-file-error-response
  test-file-error-response.cpp
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)
set_tests_properties(test-file-error-response PROPERTIES TIMEOUT 30)

gnc_add_test (test-file-chooser-async-response
  test-file-chooser-async-response.cpp
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)
set_tests_properties(test-file-chooser-async-response PROPERTIES TIMEOUT 30)

gnc_add_test(test-file-save-response test-file-save-response.cpp
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)
set_tests_properties(test-file-save-response PROPERTIES TIMEOUT 30)
gnc_add_test(test-file-open-response test-file-open-response.cpp
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)
set_tests_properties(test-file-open-response PROPERTIES TIMEOUT 30)
set_property(TEST test-file-open-response APPEND PROPERTY ENVIRONMENT
  "GSETTINGS_SCHEMA_DIR=${SCHEMADIR_BUILD}" "GSETTINGS_BACKEND=memory")
if(COMPILE_GSCHEMAS)
  add_dependencies(test-file-open-response compiled-schemas)
endif()
set_property(TEST test-file-save-response APPEND PROPERTY ENVIRONMENT
  "GSETTINGS_SCHEMA_DIR=${SCHEMADIR_BUILD}" "GSETTINGS_BACKEND=memory")
if(COMPILE_GSCHEMAS)
  add_dependencies(test-file-save-response compiled-schemas)
endif()

gnc_add_test (test-general-select-response
  test-general-select-response.cpp
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)
set_tests_properties(test-general-select-response PROPERTIES TIMEOUT 30)

gnc_add_test(test-budget-model-identity test-budget-model-identity.cpp
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)
set_tests_properties(test-budget-model-identity PROPERTIES TIMEOUT 30)

set_dist_list(test_gnome_utils_DIST
  CMakeLists.txt
  test-load-gnome-utils-module.scm
  test-dialog-object-references-response.cpp
  test-dialog-date-range-response.cpp
  test-gui-query-response.cpp
  test-input-lifetime-response.cpp
  test-dialog-account-cascade-response.cpp
  test-doclink-path-head-response.cpp
  test-gnc-autosave-response.cpp
  test-preferences-separator-response.cpp
  test-xml-encoding-assistant.cpp
  test-dialog-commodity-response.cpp
  test-account-children-response.cpp
  test-main-window-close-response.cpp
  test-tax-table-create-response.cpp
  test-file-error-response.cpp
  test-file-chooser-async-response.cpp
  test-file-save-response.cpp
  test-file-open-response.cpp
  test-general-select-response.cpp
  test-budget-model-identity.cpp)
