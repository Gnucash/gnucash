if(NOT CTEST_COMMAND)
  message(FATAL_ERROR "CTEST_COMMAND is required")
endif()

set(test_command "${CTEST_COMMAND}")
if(USE_VIRTUAL_DISPLAY)
  find_program(XVFB_RUN_EXECUTABLE xvfb-run)
  if(NOT XVFB_RUN_EXECUTABLE)
    message(FATAL_ERROR
      "xvfb-run is required to run the GnuCash tests on Linux")
  endif()
  find_program(DBUS_RUN_SESSION_EXECUTABLE dbus-run-session)
  if(NOT DBUS_RUN_SESSION_EXECUTABLE)
    message(FATAL_ERROR
      "dbus-run-session is required to run the GnuCash tests on Linux")
  endif()

  set(test_runtime_directory "${CMAKE_CURRENT_BINARY_DIR}/gnucash-test-runtime")
  file(MAKE_DIRECTORY "${test_runtime_directory}")
  file(CHMOD "${test_runtime_directory}"
    DIRECTORY_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE)

  # Start D-Bus inside xvfb-run so D-Bus activated GTK services inherit DISPLAY.
  # GTK_A11Y=none avoids an accessibility-bus activation attempt in headless CI.
  set(test_command "${CMAKE_COMMAND}" -E env
    "XDG_RUNTIME_DIR=${test_runtime_directory}" "GTK_A11Y=none"
    "${XVFB_RUN_EXECUTABLE}" -a "${DBUS_RUN_SESSION_EXECUTABLE}" --
    "${CTEST_COMMAND}")
endif()

execute_process(COMMAND ${test_command} RESULT_VARIABLE test_result)
if(NOT test_result EQUAL 0)
  message(FATAL_ERROR "CTest failed with exit code ${test_result}")
endif()
