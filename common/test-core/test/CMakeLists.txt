set(glib_log_handler_INCLUDE_DIRS ${CMAKE_BINARY_DIR}/common ${CMAKE_CURRENT_SOURCE_DIR}/..)
set(glib_log_handler_LIBS gtest PkgConfig::GLIB2)
gnc_add_test(gtest-googletest-glib-log-handler
  gtest-googletest-glib-log-handler.cpp
  glib_log_handler_INCLUDE_DIRS glib_log_handler_LIBS)

set_dist_list(test_core_test_DIST CMakeLists.txt gtest-googletest-glib-log-handler.cpp)
