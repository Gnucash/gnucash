if(NOT TEST_MODULE)
  message(FATAL_ERROR "TEST_MODULE is required")
endif()

set(reject_cases
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
list(FIND reject_cases "${TEST_CASE}" reject_case_index)
if(NOT reject_case_index EQUAL -1)
  set(child_case "${TEST_CASE}-child")
  if(TEST_CASE STREQUAL "reject-msys")
    set(expected_variable LIBINTL_INCLUDE_PATH)
  elseif(TEST_CASE STREQUAL "reject-msys-root-prefix")
    set(expected_variable ROOT_ONLY_INCLUDE)
  elseif(TEST_CASE STREQUAL "reject-msys-ldflag")
    set(expected_variable GUILE22_LDFLAGS)
  elseif(TEST_CASE STREQUAL "reject-msys-cflag")
    set(expected_variable GUILE22_CFLAGS)
  elseif(TEST_CASE MATCHES "^reject-msys-wl-ldflag-")
    set(expected_variable TARGET_WL_LINK_OPTIONS)
  elseif(TEST_CASE STREQUAL "reject-msys-shell-ldflag")
    set(expected_variable TARGET_LINK_OPTIONS)
  elseif(TEST_CASE STREQUAL "reject-msys-shell-cflag")
    set(expected_variable TARGET_COMPILE_OPTIONS)
  elseif(TEST_CASE STREQUAL "reject-msys-split-ldflag")
    set(expected_variable TARGET_LIBRARY_OPTIONS)
  elseif(TEST_CASE STREQUAL "reject-msys-split-cflag")
    set(expected_variable TARGET_INCLUDE_OPTIONS)
  elseif(TEST_CASE STREQUAL "reject-msys-genex-library")
    set(expected_variable TARGET_INTERFACE_LIBRARIES)
  elseif(TEST_CASE STREQUAL "reject-msys-genex-include")
    set(expected_variable TARGET_INTERFACE_INCLUDES)
  elseif(TEST_CASE STREQUAL "reject-probe-library")
    set(expected_variable CMAKE_REQUIRED_LIBRARIES)
  elseif(TEST_CASE STREQUAL "reject-posix-library")
    set(expected_variable TARGET_LIBRARIES)
  elseif(TEST_CASE STREQUAL "reject-posix-include")
    set(expected_variable TARGET_INCLUDE_DIRS)
  elseif(TEST_CASE STREQUAL "reject-global-c-flags")
    set(expected_variable CMAKE_C_FLAGS)
  elseif(TEST_CASE STREQUAL "reject-global-cxx-flags")
    set(expected_variable CMAKE_CXX_FLAGS)
  elseif(TEST_CASE STREQUAL "reject-global-exe-linker-flags")
    set(expected_variable CMAKE_EXE_LINKER_FLAGS)
  elseif(TEST_CASE STREQUAL "reject-global-shared-linker-flags")
    set(expected_variable CMAKE_SHARED_LINKER_FLAGS)
  elseif(TEST_CASE STREQUAL "reject-global-module-linker-flags")
    set(expected_variable CMAKE_MODULE_LINKER_FLAGS)
  elseif(TEST_CASE STREQUAL "reject-global-config-flags")
    set(expected_variable CMAKE_CXX_FLAGS_DEBUG)
  elseif(TEST_CASE STREQUAL "reject-pkg-cflags-other")
    set(expected_variable GTK4_CFLAGS_OTHER)
  elseif(TEST_CASE STREQUAL "reject-pkg-ldflags-other")
    set(expected_variable LIBOFX_LDFLAGS_OTHER)
  elseif(TEST_CASE STREQUAL "reject-runtime-loader")
    set(expected_variable WEBVIEW2_LOADER_DLL)
  elseif(TEST_CASE STREQUAL "reject-runtime-executable")
    set(expected_variable WEBVIEW2_RUNTIME_EXECUTABLE)
  else()
    set(expected_variable MINGW_DLLS)
  endif()
  execute_process(
    COMMAND "${CMAKE_COMMAND}" -DTEST_CASE=${child_case}
      -DTEST_MODULE=${TEST_MODULE} -P "${CMAKE_CURRENT_LIST_FILE}"
    RESULT_VARIABLE result
    OUTPUT_VARIABLE output
    ERROR_VARIABLE error)
  if(result EQUAL 0)
    message(FATAL_ERROR "Host input was accepted for a MinGW target")
  endif()
  if(NOT "${output}${error}" MATCHES
      "MinGW target input ${expected_variable} resolves to the host path")
    message(FATAL_ERROR "Expected host-path diagnostic was not emitted")
  endif()
  return()
endif()

set(MINGW TRUE)
set(CMAKE_HOST_WIN32 TRUE)
set(CMAKE_HOST_UNIX FALSE)
set(CMAKE_CROSSCOMPILING TRUE)
set(CMAKE_C_COMPILER "C:/host/compiler/bin/x86_64-w64-mingw32-gcc.exe")
set(CMAKE_SYSROOT "D:/target/sdk-root")
set(CMAKE_FIND_ROOT_PATH "E:/target/dependencies")
set(CMAKE_SYSTEM_PREFIX_PATH "C:/host/msys/usr")
set(SHELL "C:/host/msys/usr/bin/bash.exe")
set(ENV{MSYSTEM} UCRT64)
include("${TEST_MODULE}")

if(TEST_CASE STREQUAL "valid")
  set(Iconv_INCLUDE_DIR "D:/target/sdk-root/usr/include" CACHE PATH "")
  set(Intl_LIBRARY
    "E:/target/dependencies/lib/libintl.custom.a" CACHE FILEPATH "")
  set(LIBINTL_INCLUDE_PATH "G:/explicit-target/include" CACHE PATH "")
  set(GUILE22_INCLUDE_DIRS "/usr/x86_64-w64-mingw32/include")
  set(GUILE22_LDFLAGS "-LG:/explicit-target/lib;-lguile-2.2")
  set(WEBVIEW2_LOADER_DLL "D:/target/sdk-root/usr/bin/WebView2Loader.dll")
  set(WEBVIEW2_RUNTIME_EXECUTABLE
    "E:/target/dependencies/bin/msedgewebview2.exe")
  set(MINGW_DLLS "D:/target/sdk-root/usr/bin/libstdc++-6.dll")
  set(IGNORED_WL_OPTIONS
    "-Wl,-rpath,C:/host/msys/usr/lib;-Wl,--as-needed")

  # Build-host programs aren't target inputs and remain unrestricted.
  set(PKG_CONFIG_EXECUTABLE "C:/host/msys/usr/bin/pkg-config.exe")
  set(GUILE_EXECUTABLE "C:/host/msys/usr/bin/guile.exe")

  set(expected_sysroot "${CMAKE_SYSROOT}")
  set(expected_find_root "${CMAKE_FIND_ROOT_PATH}")
  set(expected_iconv "${Iconv_INCLUDE_DIR}")
  set(expected_intl "${Intl_LIBRARY}")
  set(expected_libintl "${LIBINTL_INCLUDE_PATH}")
  gnc_validate_mingw_target_paths(VARIABLES
    Iconv_INCLUDE_DIR Intl_LIBRARY LIBINTL_INCLUDE_PATH
    GUILE22_INCLUDE_DIRS GUILE22_LDFLAGS WEBVIEW2_LOADER_DLL
    WEBVIEW2_RUNTIME_EXECUTABLE MINGW_DLLS IGNORED_WL_OPTIONS)

  if(NOT CMAKE_SYSROOT STREQUAL expected_sysroot
      OR NOT CMAKE_FIND_ROOT_PATH STREQUAL expected_find_root
      OR NOT Iconv_INCLUDE_DIR STREQUAL expected_iconv
      OR NOT Intl_LIBRARY STREQUAL expected_intl
      OR NOT LIBINTL_INCLUDE_PATH STREQUAL expected_libintl
      OR NOT PKG_CONFIG_EXECUTABLE MATCHES "/usr/bin/pkg-config"
      OR NOT GUILE_EXECUTABLE MATCHES "/usr/bin/guile")
    message(FATAL_ERROR "Target finder configuration was modified")
  endif()
elseif(TEST_CASE STREQUAL "valid-linker-flags-preserved")
  set(CMAKE_EXE_LINKER_FLAGS "-Wl,--toolchain-cache -Wl,--nxcompat"
    CACHE STRING "Toolchain linker flags" FORCE)
  gnc_append_flags_once(CMAKE_EXE_LINKER_FLAGS
    -mwindows -Wl,--nxcompat -Wl,--dynamicbase)
  set(expected_linker_flags
    "-Wl,--toolchain-cache -Wl,--nxcompat -mwindows -Wl,--dynamicbase")
  if(NOT CMAKE_EXE_LINKER_FLAGS STREQUAL expected_linker_flags)
    message(FATAL_ERROR "Existing executable linker flags weren't preserved")
  endif()
  gnc_append_flags_once(CMAKE_EXE_LINKER_FLAGS
    -mwindows -Wl,--nxcompat -Wl,--dynamicbase)
  if(NOT CMAKE_EXE_LINKER_FLAGS STREQUAL expected_linker_flags)
    message(FATAL_ERROR "Executable linker flags weren't appended idempotently")
  endif()
  get_property(cached_linker_flags
    CACHE CMAKE_EXE_LINKER_FLAGS PROPERTY VALUE)
  if(NOT cached_linker_flags STREQUAL "-Wl,--toolchain-cache -Wl,--nxcompat")
    message(FATAL_ERROR "Executable linker flags accumulated in the cache")
  endif()
  gnc_validate_mingw_target_paths(VARIABLES CMAKE_EXE_LINKER_FLAGS)
elseif(TEST_CASE STREQUAL "valid-explicit-target-roots")
  set(CMAKE_HOST_WIN32 FALSE)
  set(CMAKE_HOST_UNIX TRUE)
  set(CMAKE_SYSROOT "/usr")
  set(CMAKE_FIND_ROOT_PATH "C:/host/msys/usr")
  set(CMAKE_SYSTEM_PREFIX_PATH "/usr;C:/host/msys")
  set(SHELL "/bin/sh")
  set(ENV{MSYSTEM} "")
  set(TARGET_INCLUDE_DIRS "/usr/include")
  set(TARGET_LIBRARIES "/usr/lib/libtarget.a")
  set(TARGET_RUNTIME "/usr/bin/libtarget.dll")
  set(WINDOWS_TARGET_INCLUDE "C:/host/msys/usr/include")
  set(WINDOWS_TARGET_LIBRARY "C:/host/msys/usr/lib/libtarget.a")
  gnc_validate_mingw_target_paths(VARIABLES
    TARGET_INCLUDE_DIRS TARGET_LIBRARIES TARGET_RUNTIME
    WINDOWS_TARGET_INCLUDE WINDOWS_TARGET_LIBRARY)
  _gnc_path_is_within("/usr2/include" "/usr" crosses_root_boundary)
  if(crosses_root_boundary)
    message(FATAL_ERROR "/usr2 must not be authorized by target root /usr")
  endif()
elseif(TEST_CASE STREQUAL "valid-posix-triplet")
  set(CMAKE_HOST_WIN32 FALSE)
  set(CMAKE_HOST_UNIX TRUE)
  set(CMAKE_SYSTEM_PREFIX_PATH "/usr;/usr/local")
  set(SHELL "/bin/sh")
  set(ENV{MSYSTEM} "")
  set(TARGET_INCLUDE_DIRS "/usr/x86_64-w64-mingw32/include")
  set(TARGET_LIBRARIES "/usr/x86_64-w64-mingw32/lib/libtarget.a")
  set(TARGET_RUNTIME "/usr/x86_64-w64-mingw32/bin/libtarget.dll")
  gnc_validate_mingw_target_paths(VARIABLES
    TARGET_INCLUDE_DIRS TARGET_LIBRARIES TARGET_RUNTIME)
elseif(TEST_CASE STREQUAL "reject-msys-child")
  set(LIBINTL_INCLUDE_PATH "C:/host/msys/usr/include")
  gnc_validate_mingw_target_paths(VARIABLES LIBINTL_INCLUDE_PATH)
elseif(TEST_CASE STREQUAL "reject-msys-root-prefix-child")
  set(CMAKE_SYSROOT "C:/")
  set(CMAKE_FIND_ROOT_PATH "")
  set(CMAKE_SYSTEM_PREFIX_PATH "C:/host/MSYS64")
  set(SHELL "")
  set(BASH "")
  set(ENV{MSYSTEM} "")
  set(ENV{MSYS2_ROOT} "")
  set(ENV{MSYS2ROOT} "")
  set(ROOT_ONLY_INCLUDE "C:/host/msys64/usr/include")
  gnc_validate_mingw_target_paths(VARIABLES ROOT_ONLY_INCLUDE)
elseif(TEST_CASE STREQUAL "reject-msys-ldflag-child")
  set(GUILE22_LDFLAGS "-LC:/host/msys/usr/lib;-lguile-2.2")
  gnc_validate_mingw_target_paths(VARIABLES GUILE22_LDFLAGS)
elseif(TEST_CASE STREQUAL "reject-msys-cflag-child")
  set(GUILE22_CFLAGS "-IC:/host/msys/usr/include;-DGUILE_STATIC")
  gnc_validate_mingw_target_paths(VARIABLES GUILE22_CFLAGS)
elseif(TEST_CASE STREQUAL "reject-msys-wl-ldflag-comma-child")
  set(TARGET_WL_LINK_OPTIONS
    "-Wl,-L,C:/host/msys/usr/lib;-lgnc-dependency")
  gnc_validate_mingw_target_paths(VARIABLES TARGET_WL_LINK_OPTIONS)
elseif(TEST_CASE STREQUAL "reject-msys-wl-ldflag-joined-child")
  set(TARGET_WL_LINK_OPTIONS
    "-Wl,-LC:/host/msys/usr/lib;-lgnc-dependency")
  gnc_validate_mingw_target_paths(VARIABLES TARGET_WL_LINK_OPTIONS)
elseif(TEST_CASE STREQUAL "reject-msys-shell-ldflag-child")
  set(TARGET_LINK_OPTIONS
    "SHELL:-L C:/host/msys/usr/lib -lgnc-dependency")
  gnc_validate_mingw_target_paths(VARIABLES TARGET_LINK_OPTIONS)
elseif(TEST_CASE STREQUAL "reject-msys-shell-cflag-child")
  set(TARGET_COMPILE_OPTIONS
    "SHELL:-I C:/host/msys/usr/include -DGNC_STATIC")
  gnc_validate_mingw_target_paths(VARIABLES TARGET_COMPILE_OPTIONS)
elseif(TEST_CASE STREQUAL "reject-msys-split-ldflag-child")
  set(TARGET_LIBRARY_OPTIONS
    "-L;C:/host/msys/usr/lib;-lgnc-dependency")
  gnc_validate_mingw_target_paths(VARIABLES TARGET_LIBRARY_OPTIONS)
elseif(TEST_CASE STREQUAL "reject-msys-split-cflag-child")
  set(TARGET_INCLUDE_OPTIONS
    "-I;C:/host/msys/usr/include;-DGNC_STATIC")
  gnc_validate_mingw_target_paths(VARIABLES TARGET_INCLUDE_OPTIONS)
elseif(TEST_CASE STREQUAL "reject-msys-genex-library-child")
  set(TARGET_INTERFACE_LIBRARIES
    "$<SHELL_PATH:C:/host/msys/usr/lib/libgnc-dependency.a>")
  gnc_validate_mingw_target_paths(VARIABLES TARGET_INTERFACE_LIBRARIES)
elseif(TEST_CASE STREQUAL "reject-msys-genex-include-child")
  set(TARGET_INTERFACE_INCLUDES
    "$<BUILD_INTERFACE:C:/host/msys/usr/include>")
  gnc_validate_mingw_target_paths(VARIABLES TARGET_INTERFACE_INCLUDES)
elseif(TEST_CASE STREQUAL "reject-probe-library-child")
  set(CMAKE_REQUIRED_LIBRARIES
    "-L;C:/host/msys/usr/lib;-lofx")
  gnc_validate_mingw_target_paths(VARIABLES CMAKE_REQUIRED_LIBRARIES)
elseif(TEST_CASE STREQUAL "reject-posix-library-child")
  set(CMAKE_HOST_WIN32 FALSE)
  set(CMAKE_HOST_UNIX TRUE)
  set(SHELL "/bin/sh")
  set(ENV{MSYSTEM} "")
  set(CMAKE_SYSROOT "")
  set(CMAKE_FIND_ROOT_PATH "")
  set(TARGET_LIBRARIES "/usr/lib/libgnc-dependency.a")
  gnc_validate_mingw_target_paths(VARIABLES TARGET_LIBRARIES)
elseif(TEST_CASE STREQUAL "reject-posix-include-child")
  set(CMAKE_HOST_WIN32 FALSE)
  set(CMAKE_HOST_UNIX TRUE)
  set(SHELL "/bin/sh")
  set(ENV{MSYSTEM} "")
  set(CMAKE_SYSROOT "/")
  set(CMAKE_FIND_ROOT_PATH "/")
  set(TARGET_INCLUDE_DIRS "/usr/include")
  gnc_validate_mingw_target_paths(VARIABLES TARGET_INCLUDE_DIRS)
elseif(TEST_CASE STREQUAL "reject-global-c-flags-child")
  set(CMAKE_C_FLAGS
    "-IC:/host/msys/usr/include -DGNC_GLOBAL_C")
  gnc_validate_mingw_target_paths(VARIABLES CMAKE_C_FLAGS)
elseif(TEST_CASE STREQUAL "reject-global-cxx-flags-child")
  set(CMAKE_CXX_FLAGS
    "-I C:/host/msys/usr/include -DGNC_GLOBAL_CXX")
  gnc_validate_mingw_target_paths(VARIABLES CMAKE_CXX_FLAGS)
elseif(TEST_CASE STREQUAL "reject-global-exe-linker-flags-child")
  set(CMAKE_EXE_LINKER_FLAGS
    "-LC:/host/msys/usr/lib -mwindows")
  gnc_validate_mingw_target_paths(VARIABLES CMAKE_EXE_LINKER_FLAGS)
elseif(TEST_CASE STREQUAL "reject-global-shared-linker-flags-child")
  set(CMAKE_SHARED_LINKER_FLAGS
    "-Wl,-L,C:/host/msys/usr/lib -Wl,--as-needed")
  gnc_validate_mingw_target_paths(VARIABLES CMAKE_SHARED_LINKER_FLAGS)
elseif(TEST_CASE STREQUAL "reject-global-module-linker-flags-child")
  set(CMAKE_MODULE_LINKER_FLAGS
    "-Wl,-LC:/host/msys/usr/lib -Wl,--as-needed")
  gnc_validate_mingw_target_paths(VARIABLES CMAKE_MODULE_LINKER_FLAGS)
elseif(TEST_CASE STREQUAL "reject-global-config-flags-child")
  set(CMAKE_BUILD_TYPE Debug)
  set(CMAKE_CXX_FLAGS_DEBUG
    "-IC:/host/msys/usr/include -DGNC_DEBUG")
  gnc_validate_mingw_target_paths(VARIABLES CMAKE_CXX_FLAGS_DEBUG)
elseif(TEST_CASE STREQUAL "reject-pkg-cflags-other-child")
  set(GTK4_CFLAGS_OTHER
    "-IC:/host/msys/usr/include;-pthread")
  gnc_validate_mingw_target_paths(VARIABLES GTK4_CFLAGS_OTHER)
elseif(TEST_CASE STREQUAL "reject-pkg-ldflags-other-child")
  set(LIBOFX_LDFLAGS_OTHER
    "-Wl,-L,C:/host/msys/usr/lib;-Wl,--as-needed")
  gnc_validate_mingw_target_paths(VARIABLES LIBOFX_LDFLAGS_OTHER)
elseif(TEST_CASE STREQUAL "reject-runtime-loader-child")
  set(WEBVIEW2_LOADER_DLL
    "C:/host/msys/usr/bin/WebView2Loader.dll")
  gnc_validate_mingw_target_paths(VARIABLES WEBVIEW2_LOADER_DLL)
elseif(TEST_CASE STREQUAL "reject-runtime-executable-child")
  set(WEBVIEW2_RUNTIME_EXECUTABLE
    "C:/host/msys/usr/bin/msedgewebview2.exe")
  gnc_validate_mingw_target_paths(VARIABLES WEBVIEW2_RUNTIME_EXECUTABLE)
elseif(TEST_CASE STREQUAL "reject-runtime-mingw-dlls-child")
  set(MINGW_DLLS
    "C:/host/msys/usr/bin/libstdc++-6.dll"
    "C:/host/msys/usr/bin/libgcc_s_seh-1.dll")
  gnc_validate_mingw_target_paths(VARIABLES MINGW_DLLS)
else()
  message(FATAL_ERROR "Unknown TEST_CASE: ${TEST_CASE}")
endif()
