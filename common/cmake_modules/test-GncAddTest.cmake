if(NOT TEST_MODULE OR NOT TEST_SCHEME_MODULE OR NOT TEST_BINARY_DIR
    OR NOT TEST_GENERATOR OR NOT TEST_GUILE_EXECUTABLE
    OR NOT TEST_GUILD_EXECUTABLE)
  message(FATAL_ERROR
    "Test modules, binary directory, and generator are required")
endif()

file(TO_CMAKE_PATH "${TEST_MODULE}" test_module)
file(TO_CMAKE_PATH "${TEST_SCHEME_MODULE}" test_scheme_module)
file(TO_CMAKE_PATH "${TEST_BINARY_DIR}/gnc-add-test-runner" test_root)
set(test_source_dir "${test_root}/source")
set(test_build_dir "${test_root}/build")
set(foreign_source_dir "${test_root}/foreign")
file(REMOVE_RECURSE "${test_root}")
file(MAKE_DIRECTORY "${test_source_dir}")
file(MAKE_DIRECTORY "${foreign_source_dir}")
file(WRITE "${test_source_dir}/test-bytecode.scm"
  "(define (run-test) #t)\n")
file(WRITE "${foreign_source_dir}/test-bytecode.scm"
  "(error \"foreign test source must not be loaded\")\n")

get_filename_component(test_guile_bin_dir
  "${TEST_GUILE_EXECUTABLE}" DIRECTORY)
get_filename_component(test_guile_prefix
  "${test_guile_bin_dir}" DIRECTORY)
file(TO_CMAKE_PATH "${test_guile_prefix}" test_guile_prefix)

file(TO_CMAKE_PATH "${TEST_CMAKE_PREFIX_PATH}" test_cmake_prefix_path)

set(fixture [=[
cmake_minimum_required(VERSION 3.16)
project(test-gnc-add-test NONE)
enable_testing()

set(WIN32 TRUE)
set(MINGW TRUE)
set(GUILE_EFFECTIVE_VERSION 2.2)
set(GUILE_REL_SITEDIR "share/guile/site")
set(GUILE_REL_UNIX_SITEDIR "share/guile/site")
set(GUILE_REL_SITECCACHEDIR "lib/guile/site-ccache")
set(GUILE_REL_UNIX_SITECCACHEDIR "lib/guile/site-ccache")
set(BINDIR_BUILD "${CMAKE_BINARY_DIR}/bin")
set(LIBDIR_BUILD "${CMAKE_BINARY_DIR}/lib")
set(DATADIR_BUILD "${CMAKE_BINARY_DIR}/share")
set(GUILE_EXECUTABLE "@TEST_GUILE_EXECUTABLE@")
set(GUILD_EXECUTABLE "@TEST_GUILD_EXECUTABLE@")
set(CMAKE_PREFIX_PATH "C:/staged;@test_cmake_prefix_path@")

include("@test_scheme_module@")
include("@test_module@")
add_custom_target(check)
add_custom_target(testbuild)
gnc_add_scheme_test_targets(scm-test-bytecode
  SOURCES "test-bytecode.scm"
  OUTPUT_DIR "tests")
gnc_add_scheme_test(test-bytecode "test-bytecode.scm")
get_guile_env()
file(WRITE "${CMAKE_BINARY_DIR}/guile-env.txt" "${GUILE_ENV}")
]=])
string(CONFIGURE "${fixture}" fixture @ONLY)
file(WRITE "${test_source_dir}/CMakeLists.txt" "${fixture}")

file(TO_CMAKE_PATH "${foreign_source_dir}" foreign_source_dir)
set(original_guile_load_path
  "${test_guile_prefix}/share/guile/2.2")
set(original_guile_load_compiled_path
  "${test_guile_prefix}/lib/guile/2.2/ccache")
if(NOT "$ENV{GUILE_LOAD_PATH}" STREQUAL "")
  list(APPEND original_guile_load_path "$ENV{GUILE_LOAD_PATH}")
endif()
if(NOT "$ENV{GUILE_LOAD_COMPILED_PATH}" STREQUAL "")
  list(APPEND original_guile_load_compiled_path
    "$ENV{GUILE_LOAD_COMPILED_PATH}")
endif()
set(ENV{GUILE_LOAD_PATH}
  "${foreign_source_dir};C:/user/site;${original_guile_load_path}")
set(ENV{GUILE_LOAD_COMPILED_PATH}
  "C:/user/cache;${original_guile_load_compiled_path}")
set(configure_command "${CMAKE_COMMAND}"
  -S "${test_source_dir}" -B "${test_build_dir}" -G "${TEST_GENERATOR}")
if(TEST_GENERATOR_PLATFORM)
  list(APPEND configure_command -A "${TEST_GENERATOR_PLATFORM}")
endif()
if(TEST_GENERATOR_TOOLSET)
  list(APPEND configure_command -T "${TEST_GENERATOR_TOOLSET}")
endif()
if(TEST_MAKE_PROGRAM)
  list(APPEND configure_command
    "-DCMAKE_MAKE_PROGRAM=${TEST_MAKE_PROGRAM}")
endif()
execute_process(
  COMMAND ${configure_command}
  RESULT_VARIABLE configure_result
  OUTPUT_VARIABLE configure_output
  ERROR_VARIABLE configure_error)
if(NOT configure_result EQUAL 0)
  message(FATAL_ERROR
    "Runner fixture configuration failed:\n${configure_output}\n${configure_error}")
endif()

file(READ "${test_build_dir}/test-bytecode-runner.scm" runner)
file(TO_CMAKE_PATH
  "${test_build_dir}/lib/guile/site-ccache/tests/test-bytecode.go"
  expected_bytecode)
set(expected_load "(load-compiled \"${expected_bytecode}\")")
string(FIND "${runner}" "${expected_load}" compiled_load_index)
if(compiled_load_index EQUAL -1)
  message(FATAL_ERROR "Windows runner doesn't load the built test bytecode")
endif()
string(FIND "${runner}" "${foreign_source_dir}" foreign_source_index)
string(FIND "${runner}" "(load-from-path" path_load_index)
string(FIND "${runner}" "(load \"" source_load_index)
if(NOT foreign_source_index EQUAL -1 OR NOT path_load_index EQUAL -1
    OR NOT source_load_index EQUAL -1)
  message(FATAL_ERROR "Windows runner contains an implicit or source fallback")
endif()

file(READ "${test_build_dir}/CTestTestfile.cmake" ctest_file)
if(NOT ctest_file MATCHES
    "test-bytecode-runner\\.scm.*--no-auto-compile|--no-auto-compile.*test-bytecode-runner\\.scm")
  message(FATAL_ERROR "CTest doesn't execute the generated runner deterministically")
endif()

file(READ "${test_build_dir}/guile-env.txt" guile_env)
string(FIND "${guile_env}"
  "${test_build_dir}/share/guile/site" build_source_index)
string(FIND "${guile_env}" "${foreign_source_dir}" foreign_source_path_index)
string(FIND "${guile_env}"
  "${test_build_dir}/lib/guile/site-ccache" build_cache_index)
string(FIND "${guile_env}" "C:/user/cache" user_cache_index)
if(build_source_index EQUAL -1 OR foreign_source_path_index EQUAL -1
    OR NOT build_source_index LESS foreign_source_path_index
    OR build_cache_index EQUAL -1 OR user_cache_index EQUAL -1
    OR NOT build_cache_index LESS user_cache_index)
  message(FATAL_ERROR "Build Scheme paths don't precede foreign paths")
endif()

if(TEST_HOST_WIN32)
  execute_process(
    COMMAND "${CMAKE_COMMAND}" --build "${test_build_dir}" --target testbuild
    RESULT_VARIABLE build_result
    OUTPUT_VARIABLE build_output
    ERROR_VARIABLE build_error)
  if(NOT build_result EQUAL 0)
    message(FATAL_ERROR
      "Building the Windows Scheme test bytecode failed:\n${build_output}\n${build_error}")
  endif()
  if(NOT EXISTS "${expected_bytecode}")
    message(FATAL_ERROR "testbuild didn't produce ${expected_bytecode}")
  endif()

  execute_process(
    COMMAND "${CMAKE_CTEST_COMMAND}" --test-dir "${test_build_dir}"
      --output-on-failure -R "^test-bytecode$"
    RESULT_VARIABLE ctest_result
    OUTPUT_VARIABLE ctest_output
    ERROR_VARIABLE ctest_error)
  if(NOT ctest_result EQUAL 0)
    message(FATAL_ERROR
      "Executing the built Windows Scheme test bytecode failed:\n${ctest_output}\n${ctest_error}")
  endif()
endif()
