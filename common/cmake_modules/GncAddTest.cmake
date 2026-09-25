

function(gnc_add_test_runtime_path _TARGET)
  if (MINGW)
    set(_runtime_path ${CMAKE_BINARY_DIR}/bin)
    if (GUILE_EXECUTABLE)
      get_filename_component(_guile_runtime_path
        "${GUILE_EXECUTABLE}" DIRECTORY)
      list(APPEND _runtime_path "${_guile_runtime_path}")
    endif()
    foreach(_prefix IN LISTS CMAKE_PREFIX_PATH)
      list(APPEND _runtime_path "${_prefix}/bin")
    endforeach()
    list(REMOVE_DUPLICATES _runtime_path)
    make_win32_path_list(_runtime_path)

    # Apply this after ENVIRONMENT so that callers' test-specific PATH values
    # remain available while every test receives the configured DLL closure.
    set_property(TEST ${_TARGET} APPEND PROPERTY ENVIRONMENT_MODIFICATION
      "PATH=path_list_prepend:${_runtime_path}")
  endif()
endfunction()
function(get_guile_env)
  set(_gnc_module_path ${LIBDIR_BUILD}:${LIBDIR_BUILD}/gnucash)
  if (WIN32)
    set(_gnc_module_path ${CMAKE_BINARY_DIR}/bin)
  endif()
  set(_relative_site_dir "${CMAKE_BINARY_DIR}/${GUILE_REL_SITEDIR}")
  set(_relative_cache_dir "${CMAKE_BINARY_DIR}/${GUILE_REL_SITECCACHEDIR}")


  set(guile_load_paths
    "${_relative_site_dir}"
    "${_relative_site_dir}/gnucash/deprecated" 
    )
  if (GUILE_COVERAGE)
    list(APPEND guile_load_paths
      "${_relative_site_dir}/gnucash"
      "${_relative_site_dir}/gnucash/report"
      "${_relative_site_dir}/gnucash/reports"
      "${_relative_site_dir}/gnucash/engine"
      "${_relative_site_dir}/gnucash/app-utils"
      "${_relative_site_dir}/gnucash/qif-import"
      )

  endif()
  if (NOT "$ENV{GUILE_LOAD_PATH}" STREQUAL "")
    list(APPEND guile_load_paths "$ENV{GUILE_LOAD_PATH}")
  endif()
  set(_guile_load_path "${guile_load_paths}")

  set(guile_load_compiled_paths
    "${_relative_cache_dir}"
    "${_relative_cache_dir}/gnucash/deprecated"
    "${_relative_cache_dir}/tests"
  )
  if (NOT "$ENV{GUILE_LOAD_COMPILED_PATH}" STREQUAL "")
    list(APPEND guile_load_compiled_paths
      "$ENV{GUILE_LOAD_COMPILED_PATH}")
  endif()
  set(_guile_load_compiled_path "${guile_load_compiled_paths}")

  if (MINGW AND ${GUILE_EFFECTIVE_VERSION} VERSION_LESS 2.2)
    set(new_path "")
    foreach(load_item ${_guile_load_path})
      make_unix_path(load_item)
      list(APPEND new_path "${load_item}")
    endforeach(load_item)
    set(_guile_load_path ${new_path})

    set(new_path "")
    foreach(load_item ${_guile_load_compiled_path})
      make_unix_path(load_item)
      list(APPEND new_path ${load_item})
    endforeach(load_item)
    set(_guile_load_compiled_path ${new_path})
  endif()

  if (NOT MINGW OR ${GUILE_EFFECTIVE_VERSION} VERSION_LESS 2.2)
    make_unix_path_list(_guile_load_path)
    make_unix_path_list(_guile_load_compiled_path)
  else()
    make_win32_path_list(_guile_load_path)
    make_win32_path_list(_guile_load_compiled_path)
  endif()

  set(_guile_env
    "GNC_MODULE_PATH=${_gnc_module_path}"
    "GUILE=${GUILE_EXECUTABLE}"
    "GUILE_LOAD_PATH=${_guile_load_path}"
    "GUILE_LOAD_COMPILED_PATH=${_guile_load_compiled_path}"
    "GUILE_AUTO_COMPILE=0"
    "GUILE_WARN_DEPRECATED=detailed"
  )
  if (APPLE)
    list(APPEND _guile_env "DYLD_LIBRARY_PATH=${_gnc_module_path}:$ENV{DYLD_LIBRARY_PATH}")
  elseif (UNIX)
    list(APPEND _guile_env "LD_LIBRARY_PATH=${_gnc_module_path}:$ENV{LD_LIBRARY_PATH}")
  endif()
  set(GUILE_ENV "${_guile_env}" PARENT_SCOPE)
endfunction()


function(gnc_add_test _TARGET _SOURCE_FILES TEST_INCLUDE_VAR_NAME TEST_LIBS_VAR_NAME)
  set(HAVE_ENV_VARS FALSE)
  if (${ARGC} GREATER 4)
    # Extra arguments are treated as environment variables
    set(HAVE_ENV_VARS TRUE)
  endif()
  set(ENVVARS "GNC_UNINSTALLED=YES;GNC_BUILDDIR=${CMAKE_BINARY_DIR}")
  if (LIBDBI_DRIVERS_DIR)
    list(APPEND ENVVARS "GNC_DBD_DIR=${LIBDBI_DRIVERS_DIR}")
  endif()
  if (HAVE_ENV_VARS)
    list(APPEND ENVVARS ${ARGN})
  endif()
  set(TEST_INCLUDE_DIRS ${${TEST_INCLUDE_VAR_NAME}})
  set(TEST_LIBS ${${TEST_LIBS_VAR_NAME}})
  set_source_files_properties (${_SOURCE_FILES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})
  if (CMAKE_GENERATOR STREQUAL Xcode)
    add_test(NAME ${_TARGET} COMMAND ${_TARGET} CONFIGURATIONS Debug;Release)
  else()
    add_test(NAME ${_TARGET} COMMAND ${_TARGET})
  endif()
  add_executable(${_TARGET} EXCLUDE_FROM_ALL ${_SOURCE_FILES})
  target_link_libraries(${_TARGET} PRIVATE ${TEST_LIBS})
if (MINGW)
    target_link_options(${_TARGET} PRIVATE -mconsole)
endif()
  target_include_directories(${_TARGET} PRIVATE ${TEST_INCLUDE_DIRS})
  set_tests_properties(${_TARGET} PROPERTIES ENVIRONMENT "${ENVVARS}$<$<CONFIG:Asan>:;ASAN_OPTIONS=${ASAN_TEST_OPTIONS}>")
  gnc_add_test_runtime_path(${_TARGET})
  add_dependencies(testbuild ${_TARGET})
endfunction()

function(gnc_add_test_with_guile _TARGET _SOURCE_FILES TEST_INCLUDE_VAR_NAME TEST_LIBS_VAR_NAME)
  get_guile_env()
  gnc_add_test(${_TARGET} "${_SOURCE_FILES}" "${TEST_INCLUDE_VAR_NAME}" "${TEST_LIBS_VAR_NAME}"
    "${GUILE_ENV}$<$<CONFIG:Asan>:;${ASAN_DYNAMIC_LIB_ENV}>;${ARGN}"
  )
endfunction()

function(gnc_add_scheme_test _TARGET _SOURCE_FILE)
  get_filename_component(_scheme_test_source "${_SOURCE_FILE}" ABSOLUTE
    BASE_DIR "${CMAKE_CURRENT_SOURCE_DIR}")
  if (WIN32)
    get_filename_component(_scheme_test_name "${_SOURCE_FILE}" NAME_WE)
    set(_scheme_test_compiled
      "${CMAKE_BINARY_DIR}/${GUILE_REL_UNIX_SITECCACHEDIR}/tests/${_scheme_test_name}.go")
    file(TO_CMAKE_PATH "${_scheme_test_compiled}" _scheme_test_compiled)
    set(_scheme_test_load "(load-compiled \"${_scheme_test_compiled}\")")
  else()
    set(_scheme_test_load "(load-from-path \"${_TARGET}\")")
  endif()
  if (GUILE_COVERAGE)
    set(_scheme_test_body "
      (set! %load-hook
          (lambda (filename)
              (when (and filename
                         (string-contains filename \"${GUILE_REL_SITEDIR}\")
                         (not (string-prefix? \"${CMAKE_BINARY_DIR}\" filename)))
                  (format #t \"%load-path = ~s~%\" %load-path)
                  (format #t \"%load-compiled-path = ~s~%\" %load-compiled-path)
                  (error \"Loading guile/site file from outside build tree!\" filename))))
      ${_scheme_test_load}
      (use-modules (system vm coverage)
                   (system vm vm))
      (call-with-values (lambda ()
          (with-code-coverage
              (lambda ()
                  (run-test))))

          (lambda (data result)
              (let ((port (open-output-file \"${coverage_dir}/${_TARGET}_results.info\")))
                  (coverage-data->lcov data port)
                  (close port))
              (exit result)))
"
    )
  else()
    set(_scheme_test_body "
      (set! %load-hook
          (lambda (filename)
              (when (and filename
                         (string-contains filename \"${GUILE_REL_SITEDIR}\")
                         (not (string-prefix? \"${CMAKE_BINARY_DIR}\" filename)))
                  (format #t \"%load-path = ~s~%\" %load-path)
                  (format #t \"%load-compiled-path = ~s~%\" %load-compiled-path)
                  (error \"Loading guile/site file from outside build tree!\" filename))))
      ${_scheme_test_load}
      (let ((result (run-test)))
           (if (boolean? result)
             (exit result)
             (exit (test-runner-fail-count result))))
"
    )
  endif()
  if (WIN32)
    # A Windows batch launcher cannot faithfully forward CTest's multiline
    # -c argument. Keep the launcher (it supplies the staged Guile runtime)
    # and pass the test program as a single script path instead.
    set(_scheme_test_runner "${CMAKE_CURRENT_BINARY_DIR}/${_TARGET}-runner.scm")
    file(GENERATE OUTPUT "${_scheme_test_runner}" CONTENT "${_scheme_test_body}")
    add_test(NAME ${_TARGET} COMMAND ${GUILE_EXECUTABLE} --no-auto-compile --debug -s "${_scheme_test_runner}")
  else()
    add_test(NAME ${_TARGET} COMMAND ${GUILE_EXECUTABLE} --debug -c "${_scheme_test_body}")
  endif()
  get_guile_env()
  set(_scheme_env "GNC_UNINSTALLED=YES;GNC_BUILDDIR=${CMAKE_BINARY_DIR}")
  if (LIBDBI_DRIVERS_DIR)
    list(APPEND _scheme_env "GNC_DBD_DIR=${LIBDBI_DRIVERS_DIR}")
  endif()
  set_tests_properties(${_TARGET} PROPERTIES ENVIRONMENT "${_scheme_env};${GUILE_ENV}$<$<CONFIG:Asan>:;${ASAN_DYNAMIC_LIB_ENV};ASAN_OPTIONS=${ASAN_TEST_OPTIONS}>;${ARGN}>")
  gnc_add_test_runtime_path(${_TARGET})
endfunction()

function(gnc_add_scheme_tests _SOURCE_FILES)
  foreach(test_file ${_SOURCE_FILES})
    get_filename_component(basename ${test_file} NAME_WE)
    gnc_add_scheme_test(${basename} ${test_file})
  endforeach()
endfunction()

function(gnc_gtest_configure)
  message(STATUS "Checking for GTEST")
  if (NOT DEFINED ${GTEST_ROOT})
    set(GTEST_ROOT $ENV{GTEST_ROOT})
  endif()
  unset(GTEST_SRC_DIR CACHE)
  if (GTEST_ROOT)
    find_path(GTEST_SRC_DIR src/gtest-all.cc NO_CMAKE_SYSTEM_PATH
      PATHS ${GTEST_ROOT}/googletest)
  endif()
  if (GTEST_SRC_DIR)
    if (EXISTS ${GTEST_SRC_DIR}/include/gtest/gtest.h)
      set(GTEST_INCLUDE_DIR ${GTEST_SRC_DIR}/include CACHE PATH "" FORCE)
    else()
      message(FATAL_ERROR "GTEST sources found, but it doesn't have 'gtest/gtest.h'")
    endif()
  else()
    if (GTEST_ROOT)
      message(FATAL_ERROR "GTEST sources not found in GTEST_ROOT")
    endif()
    find_path(GTEST_SRC_DIR src/gtest-all.cc
      PATHS /usr/src/googletest/googletest)
    if (GTEST_SRC_DIR)
      find_path(GTEST_INCLUDE_DIR gtest/gtest.h NO_CMAKE_SYSTEM_PATH
        PATHS ${GTEST_SRC_DIR}/include)
    endif()
  endif()
  find_path(GTEST_INCLUDE_DIR gtest/gtest.h)
  if (GTEST_SRC_DIR)
    set(lib_gtest_SOURCES
      "${GTEST_SRC_DIR}/src/gtest_main.cc"
      "${GTEST_SRC_DIR}/src/gtest-all.cc"
      PARENT_SCOPE)
  else()
    find_library(GTEST_SHARED_LIB gtest)
    find_library(GTEST_MAIN_LIB gtest_main)
    if (NOT (GTEST_SHARED_LIB AND GTEST_MAIN_LIB AND GTEST_INCLUDE_DIR))
      message(FATAL_ERROR "GTEST not found. Please install it or set GTEST_ROOT")
    endif()
  endif()
  set(THREADS_PREFER_PTHREAD_FLAG ON)
  find_package(Threads REQUIRED)
  set(GTEST_FOUND YES CACHE INTERNAL "Found GTest")

  message(STATUS "Checking for GMOCK")
  unset(GMOCK_SRC_DIR CACHE)
  if (GTEST_ROOT)
    find_path(GMOCK_SRC_DIR src/gmock-all.cc NO_CMAKE_SYSTEM_PATH
      PATHS ${GTEST_ROOT}/googlemock)
  endif()
  if (GMOCK_SRC_DIR)
    if (EXISTS ${GMOCK_SRC_DIR}/include/gmock/gmock.h)
      set(GMOCK_INCLUDE_DIR ${GMOCK_SRC_DIR}/include CACHE PATH "" FORCE)
    else()
      message(FATAL_ERROR "GMOCK sources found, but it doesn't have 'gmock/gmock.h'")
    endif()
  else()
    if (GTEST_ROOT)
      message(FATAL_ERROR "GMOCK sources not found in GTEST_ROOT")
    endif()
    find_path(GMOCK_SRC_DIR src/gmock-all.cc
      PATHS /usr/src/googletest/googlemock)
    if (GMOCK_SRC_DIR)
      find_path(GMOCK_INCLUDE_DIR gmock/gmock.h NO_CMAKE_SYSTEM_PATH
        PATHS ${GMOCK_SRC_DIR}/include)
    endif()
  endif()
  find_path(GMOCK_INCLUDE_DIR gmock/gmock.h)
  if (GMOCK_SRC_DIR)
    set(GMOCK_SRC "${GMOCK_SRC_DIR}/src/gmock-all.cc" PARENT_SCOPE)
    set(GMOCK_LIB "${CMAKE_BINARY_DIR}/common/test-core/libgmock.a" PARENT_SCOPE)
  else()
    find_library(GMOCK_SHARED_LIB gmock)
    find_library(GMOCK_MAIN_LIB gmock_main)
    if (GMOCK_MAIN_LIB AND GMOCK_SHARED_LIB AND GMOCK_INCLUDE_DIR)
      set(GMOCK_LIB "${GMOCK_SHARED_LIB};${GMOCK_MAIN_LIB}" PARENT_SCOPE)
    else()
      message(FATAL_ERROR "GMOCK not found. Please install it or set GTEST_ROOT")
    endif()
  endif()
  gnc_validate_mingw_target_paths(VARIABLES
    GTEST_SRC_DIR GTEST_INCLUDE_DIR GTEST_SHARED_LIB GTEST_MAIN_LIB
    GMOCK_SRC_DIR GMOCK_INCLUDE_DIR GMOCK_SHARED_LIB GMOCK_MAIN_LIB)

  set(GMOCK_FOUND YES PARENT_SCOPE)
endfunction()
