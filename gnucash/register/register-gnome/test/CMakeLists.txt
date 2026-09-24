set(REGISTER_GNOME_TEST_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common
  ${CMAKE_SOURCE_DIR}/gnucash/register/register-gnome
  ${CMAKE_SOURCE_DIR}/libgnucash/core-utils
)

set(REGISTER_GNOME_TEST_LIBS
  gnc-core-utils
  gnc-register-gnome
)

set(test_gnc_completion_model_SOURCES
  test-gnc-completion-model.c
)

set(test_gnc_register_editor_size_SOURCES
  test-gnc-register-editor-size.c
)

set(test_gnucash_date_picker_SOURCES
  test-gnucash-date-picker.c
)

gnc_add_test(test-gnc-completion-model
  "${test_gnc_completion_model_SOURCES}"
  REGISTER_GNOME_TEST_INCLUDE_DIRS
  REGISTER_GNOME_TEST_LIBS
)

gnc_add_test(test-gnc-register-editor-size
  "${test_gnc_register_editor_size_SOURCES}"
  REGISTER_GNOME_TEST_INCLUDE_DIRS
  REGISTER_GNOME_TEST_LIBS
)

gnc_add_test(test-gnucash-date-picker
  "${test_gnucash_date_picker_SOURCES}"
  REGISTER_GNOME_TEST_INCLUDE_DIRS
  REGISTER_GNOME_TEST_LIBS
)

target_compile_definitions(test-gnc-register-editor-size PRIVATE
  GNC_SOURCE_CSS="${CMAKE_SOURCE_DIR}/gnucash/gnucash.css"
)

set_dist_list(test_register_gnome_DIST
  CMakeLists.txt
  ${test_gnc_completion_model_SOURCES}
  ${test_gnc_register_editor_size_SOURCES}
  ${test_gnucash_date_picker_SOURCES}
)
