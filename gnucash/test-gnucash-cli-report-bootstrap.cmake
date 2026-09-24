if(NOT GNUCASH_CLI_EXECUTABLE)
  message(FATAL_ERROR "GNUCASH_CLI_EXECUTABLE is required")
endif()

if(NOT BUILD_ROOT)
  message(FATAL_ERROR "BUILD_ROOT is required")
endif()

if(NOT GUILE_RUNTIME_LIBDIR OR NOT GUILE_RUNTIME_CCACHEDIR)
  message(FATAL_ERROR "Guile runtime directories are required")
endif()

set(test_config_root
  "${BUILD_ROOT}/Testing/Temporary/test-gnucash-cli-report-bootstrap")
set(list_config_home "${test_config_root}/list")
set(failure_config_home "${test_config_root}/failure")
file(REMOVE_RECURSE "${test_config_root}")
file(MAKE_DIRECTORY "${list_config_home}" "${failure_config_home}")

unset(ENV{GUILE_LOAD_PATH})
unset(ENV{GUILE_LOAD_COMPILED_PATH})
unset(ENV{GUILE_AUTO_COMPILE})
unset(ENV{GNC_MODULE_PATH})
set(ENV{GUILE_LIBS} "${GUILE_RUNTIME_LIBDIR}")
set(ENV{GUILE_COMPILED_LIBS} "${GUILE_RUNTIME_CCACHEDIR}")
set(ENV{GNC_UNINSTALLED} YES)
set(ENV{GNC_BUILDDIR} "${BUILD_ROOT}")
set(ENV{LC_ALL} C)

set(ENV{GNC_CONFIG_HOME} "${list_config_home}")
execute_process(
  COMMAND "${GNUCASH_CLI_EXECUTABLE}" --report list
  RESULT_VARIABLE cli_result
  OUTPUT_VARIABLE cli_stdout
  ERROR_VARIABLE cli_stderr
  TIMEOUT 60
  ENCODING UTF-8
)

if(NOT cli_result EQUAL 0)
  message(FATAL_ERROR
    "gnucash-cli --report list failed with ${cli_result}\n"
    "stdout:\n${cli_stdout}\n"
    "stderr:\n${cli_stderr}")
endif()

string(CONCAT cli_output "${cli_stdout}\n${cli_stderr}")
if(cli_output MATCHES "Cannot exit gracefully when init is in progress")
  message(FATAL_ERROR
    "gnucash-cli left Guile while initialization was in progress:\n"
    "${cli_output}")
endif()

if(NOT cli_stdout MATCHES "(^|\n)\\* [ C] ")
  message(FATAL_ERROR
    "gnucash-cli did not print the expected report list\n"
    "stdout:\n${cli_stdout}\n"
    "stderr:\n${cli_stderr}")
endif()

set(ENV{GNC_CONFIG_HOME} "${failure_config_home}")
execute_process(
  COMMAND "${GNUCASH_CLI_EXECUTABLE}" --report run
      --name __gnucash_missing_report__ __gnucash_missing_file__
  RESULT_VARIABLE failure_result
  OUTPUT_VARIABLE failure_stdout
  ERROR_VARIABLE failure_stderr
  TIMEOUT 60
  ENCODING UTF-8
)

if(NOT failure_result EQUAL 1)
  message(FATAL_ERROR
    "gnucash-cli returned ${failure_result} instead of 1 for an unknown report\n"
    "stdout:\n${failure_stdout}\n"
    "stderr:\n${failure_stderr}")
endif()

string(CONCAT failure_output "${failure_stdout}\n${failure_stderr}")
if(failure_output MATCHES "Cannot exit gracefully when init is in progress")
  message(FATAL_ERROR
    "gnucash-cli left Guile while handling a report error:\n"
    "${failure_output}")
endif()
