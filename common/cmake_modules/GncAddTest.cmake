

function(get_guile_env)
  set(_gnc_module_path ${LIBDIR_BUILD}:${LIBDIR_BUILD}/gnucash)
  if (WIN32)
    set(_gnc_module_path ${CMAKE_BINARY_DIR}/bin)
  endif()
  set(_relative_site_dir "${CMAKE_BINARY_DIR}/${GUILE_REL_SITEDIR}")
  set(_relative_cache_dir "${CMAKE_BINARY_DIR}/${GUILE_REL_SITECCACHEDIR}")

  if (MINGW64)
    set(fpath "")
    set(path $ENV{PATH})
    list(INSERT path 0 ${CMAKE_BINARY_DIR}/bin)
    if (${GUILE_EFFECTIVE_VERSION} VERSION_LESS 2.2)
      foreach(dir ${path})
        make_unix_path(dir)
        list(APPEND fpath ${dir})
      endforeach(dir)
      make_unix_path_list(fpath)
    else()
      set(fpath ${path})
      make_win32_path_list(fpath)
    endif()
  endif()

  set(guile_load_paths "$ENV{GUILE_LOAD_PATH}")
  list(APPEND guile_load_paths
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
  set(_guile_load_path "${guile_load_paths}")

  set(guile_load_compiled_paths "$ENV{GUILE_LOAD_COMPILED_PATH}")
  list(APPEND guile_load_compiled_paths
    "${_relative_cache_dir}"
    "${_relative_cache_dir}/gnucash/deprecated"
    "${_relative_cache_dir}/tests"
  )
  set(_guile_load_compiled_path "${guile_load_compiled_paths}")

  if (MINGW64 AND ${GUILE_EFFECTIVE_VERSION} VERSION_LESS 2.2)
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

  if (NOT MINGW64 OR ${GUILE_EFFECTIVE_VERSION} VERSION_LESS 2.2)
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
    "GUILE_WARN_DEPRECATED=detailed"
  )
  if (MINGW64)
    list(APPEND _guile_env "PATH=${fpath}")
  elseif (APPLE)
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
  add_dependencies(testbuild ${_TARGET})
endfunction()

function(gnc_add_test_with_guile _TARGET _SOURCE_FILES TEST_INCLUDE_VAR_NAME TEST_LIBS_VAR_NAME)
  get_guile_env()
  gnc_add_test(${_TARGET} "${_SOURCE_FILES}" "${TEST_INCLUDE_VAR_NAME}" "${TEST_LIBS_VAR_NAME}"
    "${GUILE_ENV}$<$<CONFIG:Asan>:;${ASAN_DYNAMIC_LIB_ENV}>;${ARGN}"
  )
endfunction()

function(gnc_add_scheme_test _TARGET _SOURCE_FILE)
  if (GUILE_COVERAGE)
    add_test(NAME ${_TARGET} COMMAND ${GUILE_EXECUTABLE} --debug -c "
      (set! %load-hook
          (lambda (filename)
              (when (and filename
                         (string-contains filename \"${GUILE_REL_SITEDIR}\")
                         (not (string-prefix? \"${CMAKE_BINARY_DIR}\" filename)))
                  (format #t \"%load-path = ~s~%\" %load-path)
                  (format #t \"%load-compiled-path = ~s~%\" %load-compiled-path)
                  (error \"Loading guile/site file from outside build tree!\" filename))))
      (load-from-path \"${_TARGET}\")
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
    add_test(NAME ${_TARGET} COMMAND ${GUILE_EXECUTABLE} --debug -c "
      (set! %load-hook
          (lambda (filename)
              (when (and filename
                         (string-contains filename \"${GUILE_REL_SITEDIR}\")
                         (not (string-prefix? \"${CMAKE_BINARY_DIR}\" filename)))
                  (format #t \"%load-path = ~s~%\" %load-path)
                  (format #t \"%load-compiled-path = ~s~%\" %load-compiled-path)
                  (error \"Loading guile/site file from outside build tree!\" filename))))
      (load-from-path \"${_TARGET}\")
      (let ((result (run-test)))
           (if (boolean? result)
             (exit result)
             (exit (test-runner-fail-count result))))
"
    )
  endif()
  get_guile_env()
  set_tests_properties(${_TARGET} PROPERTIES ENVIRONMENT "${GUILE_ENV}$<$<CONFIG:Asan>:;${ASAN_DYNAMIC_LIB_ENV};ASAN_OPTIONS=${ASAN_TEST_OPTIONS}>;${ARGN}>")
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
  set(GMOCK_FOUND YES PARENT_SCOPE)
endfunction()
