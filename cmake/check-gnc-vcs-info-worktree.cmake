if (NOT DEFINED SOURCE_ROOT OR NOT DEFINED SHELL OR
    NOT DEFINED GIT_EXECUTABLE OR NOT DEFINED TEST_ROOT)
  message(FATAL_ERROR "SOURCE_ROOT, SHELL, GIT_EXECUTABLE, and TEST_ROOT are required")
endif()

file(REMOVE_RECURSE "${TEST_ROOT}")
file(MAKE_DIRECTORY "${TEST_ROOT}/repository" "${TEST_ROOT}/outside")
set(REPOSITORY "${TEST_ROOT}/repository")

execute_process(
  COMMAND "${GIT_EXECUTABLE}" init --quiet "${REPOSITORY}"
  RESULT_VARIABLE RESULT)
if (NOT RESULT EQUAL 0)
  message(FATAL_ERROR "Unable to initialize the temporary Git repository")
endif()

file(WRITE "${REPOSITORY}/tracked.txt" "tracked\n")
execute_process(
  COMMAND "${GIT_EXECUTABLE}" -C "${REPOSITORY}" add tracked.txt
  RESULT_VARIABLE RESULT)
if (NOT RESULT EQUAL 0)
  message(FATAL_ERROR "Unable to stage the temporary repository")
endif()

execute_process(
  COMMAND "${GIT_EXECUTABLE}" -C "${REPOSITORY}"
    -c user.name=GnuCash
    -c user.email=gnucash@example.invalid
    commit --quiet -m "Test revision"
  RESULT_VARIABLE RESULT)
if (NOT RESULT EQUAL 0)
  message(FATAL_ERROR "Unable to commit the temporary repository")
endif()

execute_process(
  COMMAND "${GIT_EXECUTABLE}" -C "${REPOSITORY}"
    log -1 --pretty=format:%h HEAD
  OUTPUT_VARIABLE EXPECTED_REVISION
  OUTPUT_STRIP_TRAILING_WHITESPACE
  RESULT_VARIABLE RESULT)
if (NOT RESULT EQUAL 0)
  message(FATAL_ERROR "Unable to read the temporary repository revision")
endif()

execute_process(
  COMMAND "${CMAKE_COMMAND}" -E env "GIT_CMD=${GIT_EXECUTABLE}"
    "${SHELL}" "${SOURCE_ROOT}/util/gnc-vcs-info" -r "${REPOSITORY}"
  WORKING_DIRECTORY "${TEST_ROOT}/outside"
  OUTPUT_VARIABLE ACTUAL_REVISION
  OUTPUT_STRIP_TRAILING_WHITESPACE
  RESULT_VARIABLE RESULT)
if (NOT RESULT EQUAL 0)
  message(FATAL_ERROR "gnc-vcs-info failed outside of the source directory")
endif()

file(REMOVE_RECURSE "${TEST_ROOT}")

if (NOT ACTUAL_REVISION STREQUAL EXPECTED_REVISION)
  message(FATAL_ERROR
    "Expected clean revision '${EXPECTED_REVISION}', got '${ACTUAL_REVISION}'")
endif()
