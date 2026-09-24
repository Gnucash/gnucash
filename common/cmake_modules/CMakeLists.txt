
set(cmake_FILES
  GncAddGSchemaTargets.cmake
  GncAddSchemeTargets.cmake
  GncAddSwigCommand.cmake
  GncAddTest.cmake
  GncFindLibm.cmake
  GncGenerateGResources.cmake
  GncMinGWTargetPaths.cmake
  MacroAddSourceFileCompileFlags.cmake
  MacroAppendForeach.cmake
  MakeDist.cmake
  MakeDistFiles.cmake
  MakeDistCheck.cmake
)

set(_gnc_mingw_target_path_test_cases
  valid
  valid-linker-flags-preserved
  valid-explicit-target-roots
  valid-posix-triplet
  reject-msys
  reject-msys-root-prefix
  reject-msys-ldflag
  reject-msys-cflag
  reject-msys-wl-ldflag-comma
  reject-msys-wl-ldflag-joined
  reject-msys-shell-ldflag
  reject-msys-shell-cflag
  reject-msys-split-ldflag
  reject-msys-split-cflag
  reject-msys-genex-library
  reject-msys-genex-include
  reject-probe-library
  reject-posix-library
  reject-posix-include
  reject-global-c-flags
  reject-global-cxx-flags
  reject-global-exe-linker-flags
  reject-global-shared-linker-flags
  reject-global-module-linker-flags
  reject-global-config-flags
  reject-pkg-cflags-other
  reject-pkg-ldflags-other
  reject-runtime-loader
  reject-runtime-executable
  reject-runtime-mingw-dlls)
foreach(_test_case IN LISTS _gnc_mingw_target_path_test_cases)
  add_test(NAME test-gnc-mingw-target-paths-${_test_case}
    COMMAND ${CMAKE_COMMAND}
      -DTEST_CASE=${_test_case}
      -DTEST_MODULE=${CMAKE_CURRENT_SOURCE_DIR}/GncMinGWTargetPaths.cmake
      -P ${CMAKE_CURRENT_SOURCE_DIR}/test-GncMinGWTargetPaths.cmake)
endforeach()
unset(_gnc_mingw_target_path_test_cases)

add_test(NAME test-gnc-add-test-windows-scheme-runner
  COMMAND ${CMAKE_COMMAND}
    -DTEST_MODULE=${CMAKE_CURRENT_SOURCE_DIR}/GncAddTest.cmake
    -DTEST_SCHEME_MODULE=${CMAKE_CURRENT_SOURCE_DIR}/GncAddSchemeTargets.cmake
    -DTEST_BINARY_DIR=${CMAKE_CURRENT_BINARY_DIR}
    -DTEST_GENERATOR=${CMAKE_GENERATOR}
    -DTEST_GENERATOR_PLATFORM=${CMAKE_GENERATOR_PLATFORM}
    -DTEST_GENERATOR_TOOLSET=${CMAKE_GENERATOR_TOOLSET}
    -DTEST_MAKE_PROGRAM=${CMAKE_MAKE_PROGRAM}
    -DTEST_GUILE_EXECUTABLE=${GUILE_EXECUTABLE}
    -DTEST_GUILD_EXECUTABLE=${GUILD_EXECUTABLE}
    "-DTEST_CMAKE_PREFIX_PATH=${CMAKE_PREFIX_PATH}"
    -DTEST_HOST_WIN32=${WIN32}
    -P ${CMAKE_CURRENT_SOURCE_DIR}/test-GncAddTest.cmake)

add_test(NAME test-gnc-add-scheme-targets-relative-install-dirs
  COMMAND ${CMAKE_COMMAND}
    -DTEST_MODULE=${CMAKE_CURRENT_SOURCE_DIR}/GncAddSchemeTargets.cmake
    -DTEST_MINGW=${MINGW}
    -DTEST_GUILE_PREFIX=${GUILE_PREFIX}
    -DTEST_GUILE_SITEDIR=${GUILE_SITEDIR}
    -DTEST_GUILE_SITECCACHEDIR=${GUILE_SITECCACHEDIR}
    -DTEST_GUILE_REL_DATADIR=${GUILE_REL_DATADIR}
    -DTEST_GUILE_REL_LIBDIR=${GUILE_REL_LIBDIR}
    -DTEST_GUILE_REL_CCACHEDIR=${GUILE_REL_CCACHEDIR}
    -DTEST_GUILE_REL_SITEDIR=${GUILE_REL_SITEDIR}
    -DTEST_GUILE_REL_SITECCACHEDIR=${GUILE_REL_SITECCACHEDIR}
    -P ${CMAKE_CURRENT_SOURCE_DIR}/test-GncAddSchemeTargets.cmake)

set_dist_list(cmake_modules_DIST CMakeLists.txt COPYING-CMAKE-SCRIPTS.txt
  test-GncAddSchemeTargets.cmake test-GncAddTest.cmake
  test-GncMinGWTargetPaths.cmake ${cmake_FILES})
