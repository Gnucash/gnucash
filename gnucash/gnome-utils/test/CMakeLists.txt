
set(GNOME_UTILS_GUI_TEST_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common
  ${CMAKE_SOURCE_DIR}/gnucash/gnome-utils
  ${CMAKE_SOURCE_DIR}/libgnucash/engine
  ${CMAKE_SOURCE_DIR}/libgnucash/core-utils
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


set (test_gnc_file_dialog_SOURCES
  test-gnc-file-dialog.c
)

gnc_add_test (test-gnc-file-dialog "${test_gnc_file_dialog_SOURCES}"
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)


set (test_gnc_file_load_cancel_SOURCES
  test-gnc-file-load-cancel.c
)

gnc_add_test (test-gnc-file-load-cancel "${test_gnc_file_load_cancel_SOURCES}"
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS
  GNC_TEST_FILES=${CMAKE_SOURCE_DIR}/libgnucash/backend/xml/test/test-files/xml2)
add_dependencies (test-gnc-file-load-cancel gncmod-backend-xml)

set (test_gnc_session_transition_SOURCES
  test-gnc-session-transition.c
)

gnc_add_test (test-gnc-session-transition
  "${test_gnc_session_transition_SOURCES}"
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)

set (test_gnc_session_load_executor_SOURCES
  test-gnc-session-load-executor.c
)

gnc_add_test (test-gnc-session-load-executor
  "${test_gnc_session_load_executor_SOURCES}"
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)

set (test_gnc_session_load_executor_no_assert_SOURCES
  test-gnc-session-load-executor-no-assert.c
  ../gnc-session-load-executor.c
)
set (SESSION_LOAD_EXECUTOR_TEST_LIBS
  PkgConfig::GLIB2
)

gnc_add_test (test-gnc-session-load-executor-no-assert
  "${test_gnc_session_load_executor_no_assert_SOURCES}"
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS SESSION_LOAD_EXECUTOR_TEST_LIBS)
target_compile_definitions (test-gnc-session-load-executor-no-assert
  PRIVATE G_DISABLE_ASSERT)


set (test_gnc_account_type_list_SOURCES
  test-gnc-account-type-list.c
)

gnc_add_test (test-gnc-account-type-list "${test_gnc_account_type_list_SOURCES}"
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)

set (test_gnc_account_sel_lifecycle_SOURCES
  test-gnc-account-sel-lifecycle.c
)

gnc_add_test (test-gnc-account-sel-lifecycle
  "${test_gnc_account_sel_lifecycle_SOURCES}"
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS
  GSETTINGS_SCHEMA_DIR=${CMAKE_BINARY_DIR}/share/glib-2.0/schemas)

set (test_dialog_account_type_selection_SOURCES
  test-dialog-account-type-selection.c
)

gnc_add_test (test-dialog-account-type-selection
  "${test_dialog_account_type_selection_SOURCES}"
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS
  GSETTINGS_SCHEMA_DIR=${CMAKE_BINARY_DIR}/share/glib-2.0/schemas)

set (test_gnc_tree_view_SOURCES
  test-gnc-tree-view.c
)

gnc_add_test (test-gnc-tree-view "${test_gnc_tree_view_SOURCES}"
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)

set (test_tree_view_row_ownership_SOURCES
  test-tree-view-row-ownership.c
)

gnc_add_test (test-tree-view-row-ownership
  "${test_tree_view_row_ownership_SOURCES}"
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS
  GSETTINGS_SCHEMA_DIR=${CMAKE_BINARY_DIR}/share/glib-2.0/schemas)

set (test_gnc_register_input_SOURCES
  test-gnc-register-input.c
)

gnc_add_test (test-gnc-register-input "${test_gnc_register_input_SOURCES}"
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)

set (test_gnc_statusbar_SOURCES
  test-gnc-statusbar.c
)

gnc_add_test (test-gnc-statusbar "${test_gnc_statusbar_SOURCES}"
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)

set (test_gnc_toolbar_SOURCES
  test-gnc-toolbar.c
)

gnc_add_test (test-gnc-toolbar "${test_gnc_toolbar_SOURCES}"
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS
  GNC_UI_SOURCE_DIR=${CMAKE_SOURCE_DIR}/gnucash/ui)

set (test_gnc_window_application_SOURCES
  test-gnc-window-application.c
)

gnc_add_test (test-gnc-window-application
  "${test_gnc_window_application_SOURCES}"
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)

set (test_gnucash_guile_bootstrap_SOURCES
  test-gnucash-guile-bootstrap.c
  ${CMAKE_SOURCE_DIR}/gnucash/gnucash-guile-bootstrap.c
)

set (GUILE_APPLICATION_TEST_INCLUDE_DIRS
  ${GNOME_UTILS_GUI_TEST_INCLUDE_DIRS}
  ${CMAKE_SOURCE_DIR}/gnucash
)

set (GUILE_APPLICATION_TEST_LIBS
  PkgConfig::GTK4
  PkgConfig::GLIB2
  ${GUILE_LDFLAGS}
)

gnc_add_test_with_guile (test-gnucash-guile-bootstrap
  "${test_gnucash_guile_bootstrap_SOURCES}"
  GUILE_APPLICATION_TEST_INCLUDE_DIRS GUILE_APPLICATION_TEST_LIBS)

set (test_gnc_accelerators_SOURCES
  test-gnc-accelerators.c
)

gnc_add_test (test-gnc-accelerators "${test_gnc_accelerators_SOURCES}"
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)

set (test_gnc_dense_cal_SOURCES
  test-gnc-dense-cal.c
)

gnc_add_test (test-gnc-dense-cal "${test_gnc_dense_cal_SOURCES}"
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)

set (test_gnc_date_edit_calendar_SOURCES
  test-gnc-date-edit-calendar.c
)

gnc_add_test (test-gnc-date-edit-calendar
  "${test_gnc_date_edit_calendar_SOURCES}"
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)

set (test_gnc_dropdown_width_SOURCES
  test-gnc-dropdown-width.c
)

gnc_add_test (test-gnc-dropdown-width "${test_gnc_dropdown_width_SOURCES}"
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)

set (test_gnc_period_select_SOURCES
  test-gnc-period-select.c
)

gnc_add_test (test-gnc-period-select "${test_gnc_period_select_SOURCES}"
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS
  GSETTINGS_SCHEMA_DIR=${CMAKE_BINARY_DIR}/share/glib-2.0/schemas)

set (test_gnc_currency_edit_SOURCES
  test-gnc-currency-edit.c
)

gnc_add_test (test-gnc-currency-edit "${test_gnc_currency_edit_SOURCES}"
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)

set (test_doclink_view_SOURCES
  test-doclink-view.c
)

gnc_add_test (test-doclink-view "${test_doclink_view_SOURCES}"
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)

set (test_dialog_transfer_model_lifecycle_SOURCES
  test-dialog-transfer-model-lifecycle.c
)

gnc_add_test (test-dialog-transfer-model-lifecycle
  "${test_dialog_transfer_model_lifecycle_SOURCES}"
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS
  GSETTINGS_SCHEMA_DIR=${CMAKE_BINARY_DIR}/share/glib-2.0/schemas)

set (test_dialog_tax_table_column_ownership_SOURCES
  test-dialog-tax-table-column-ownership.c
)

gnc_add_test (test-dialog-tax-table-column-ownership
  "${test_dialog_tax_table_column_ownership_SOURCES}"
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS
  GSETTINGS_SCHEMA_DIR=${CMAKE_BINARY_DIR}/share/glib-2.0/schemas)

set (test_tree_view_column_ownership_SOURCES
  test-tree-view-column-ownership.c
)

set (TREE_VIEW_COLUMN_OWNERSHIP_TEST_INCLUDE_DIRS
  ${GNOME_UTILS_GUI_TEST_INCLUDE_DIRS}
  ${CMAKE_SOURCE_DIR}/libgnucash/app-utils
)
set (TREE_VIEW_COLUMN_OWNERSHIP_TEST_LIBS
  ${GNOME_UTILS_GUI_TEST_LIBS}
  gnc-app-utils
)

gnc_add_test (test-tree-view-column-ownership
  "${test_tree_view_column_ownership_SOURCES}"
  TREE_VIEW_COLUMN_OWNERSHIP_TEST_INCLUDE_DIRS
  TREE_VIEW_COLUMN_OWNERSHIP_TEST_LIBS
  GSETTINGS_SCHEMA_DIR=${CMAKE_BINARY_DIR}/share/glib-2.0/schemas)

set (test_gnc_scrub_job_runner_SOURCES
  test-gnc-scrub-job-runner.c
)

gnc_add_test (test-gnc-scrub-job-runner "${test_gnc_scrub_job_runner_SOURCES}"
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)

set (test_book_options_dialog_lifecycle_SOURCES
  test-book-options-dialog-lifecycle.cpp
)

set (BOOK_OPTIONS_DIALOG_TEST_INCLUDE_DIRS
  ${GNOME_UTILS_GUI_TEST_INCLUDE_DIRS}
  ${CMAKE_SOURCE_DIR}/gnucash/gnome
)

set (BOOK_OPTIONS_DIALOG_TEST_LIBS
  ${GNOME_UTILS_GUI_TEST_LIBS}
  gnc-gnome
  ${GUILE_LDFLAGS}
)

gnc_add_test_with_guile (test-book-options-dialog-lifecycle
  "${test_book_options_dialog_lifecycle_SOURCES}"
  BOOK_OPTIONS_DIALOG_TEST_INCLUDE_DIRS BOOK_OPTIONS_DIALOG_TEST_LIBS
  GSETTINGS_SCHEMA_DIR=${CMAKE_BINARY_DIR}/share/glib-2.0/schemas)

set (test_assistant_hierarchy_layout_SOURCES
  test-assistant-hierarchy-layout.c
)

gnc_add_test (test-assistant-hierarchy-layout
  "${test_assistant_hierarchy_layout_SOURCES}"
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)

set (test_pixmap_option_SOURCES
  test-pixmap-option.cpp
)

gnc_add_test (test-pixmap-option "${test_pixmap_option_SOURCES}"
  GNOME_UTILS_GUI_TEST_INCLUDE_DIRS GNOME_UTILS_GUI_TEST_LIBS)


set_dist_list(test_gnome_utils_DIST
  CMakeLists.txt
  test-load-gnome-utils-module.scm
  ${test_gnc_file_dialog_SOURCES}
  ${test_gnc_file_load_cancel_SOURCES}
  ${test_gnc_session_transition_SOURCES}
  ${test_gnc_session_load_executor_SOURCES}
  test-gnc-session-load-executor-no-assert.c
  ${test_gnc_account_type_list_SOURCES}
  ${test_gnc_account_sel_lifecycle_SOURCES}
  ${test_dialog_account_type_selection_SOURCES}
  ${test_gnc_tree_view_SOURCES}
  ${test_tree_view_row_ownership_SOURCES}
  ${test_gnc_register_input_SOURCES}
  ${test_gnc_statusbar_SOURCES}
  ${test_gnc_toolbar_SOURCES}
  ${test_gnc_window_application_SOURCES}
  test-gnucash-guile-bootstrap.c
  ${test_gnc_accelerators_SOURCES}
  ${test_gnc_dense_cal_SOURCES}
  ${test_gnc_date_edit_calendar_SOURCES}
  ${test_gnc_dropdown_width_SOURCES}
  ${test_gnc_period_select_SOURCES}
  ${test_gnc_currency_edit_SOURCES}
  ${test_doclink_view_SOURCES}
  ${test_dialog_transfer_model_lifecycle_SOURCES}
  ${test_dialog_tax_table_column_ownership_SOURCES}
  ${test_tree_view_column_ownership_SOURCES}
  ${test_gnc_scrub_job_runner_SOURCES}
  ${test_book_options_dialog_lifecycle_SOURCES}
  ${test_assistant_hierarchy_layout_SOURCES}
  ${test_pixmap_option_SOURCES}
)
