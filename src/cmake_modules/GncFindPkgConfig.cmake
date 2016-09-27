if((${CMAKE_VERSION} VERSION_GREATER 3.1) AND (${CMAKE_VERSION} VERSION_LESS 3.5))

function (pkg_get_variable result pkg variable)
  _pkgconfig_invoke("${pkg}" "prefix" "result" "" "--variable=${variable}")
  set("${result}"
          "${prefix_result}"
          PARENT_SCOPE)
endfunction ()


macro(_gnc_pkg_check_modules_internal _is_required _is_silent _no_cmake_path _no_cmake_environment_path _prefix)
  _pkgconfig_unset(${_prefix}_FOUND)
  _pkgconfig_unset(${_prefix}_VERSION)
  _pkgconfig_unset(${_prefix}_PREFIX)
  _pkgconfig_unset(${_prefix}_INCLUDEDIR)
  _pkgconfig_unset(${_prefix}_LIBDIR)
  _pkgconfig_unset(${_prefix}_LIBS)
  _pkgconfig_unset(${_prefix}_LIBS_L)
  _pkgconfig_unset(${_prefix}_LIBS_PATHS)
  _pkgconfig_unset(${_prefix}_LIBS_OTHER)
  _pkgconfig_unset(${_prefix}_CFLAGS)
  _pkgconfig_unset(${_prefix}_CFLAGS_I)
  _pkgconfig_unset(${_prefix}_CFLAGS_OTHER)
  _pkgconfig_unset(${_prefix}_STATIC_LIBDIR)
  _pkgconfig_unset(${_prefix}_STATIC_LIBS)
  _pkgconfig_unset(${_prefix}_STATIC_LIBS_L)
  _pkgconfig_unset(${_prefix}_STATIC_LIBS_PATHS)
  _pkgconfig_unset(${_prefix}_STATIC_LIBS_OTHER)
  _pkgconfig_unset(${_prefix}_STATIC_CFLAGS)
  _pkgconfig_unset(${_prefix}_STATIC_CFLAGS_I)
  _pkgconfig_unset(${_prefix}_STATIC_CFLAGS_OTHER)

  # create a better addressable variable of the modules and calculate its size
  set(_pkg_check_modules_list ${ARGN})
  list(LENGTH _pkg_check_modules_list _pkg_check_modules_cnt)

  if(PKG_CONFIG_EXECUTABLE)
    # give out status message telling checked module
    if (NOT ${_is_silent})
      if (_pkg_check_modules_cnt EQUAL 1)
        message(STATUS "Checking for module '${_pkg_check_modules_list}'")
      else()
        message(STATUS "Checking for modules '${_pkg_check_modules_list}'")
      endif()
    endif()

    set(_pkg_check_modules_packages)
    set(_pkg_check_modules_failed)

    set(_extra_paths)

    if(NOT _no_cmake_path)
      _pkgconfig_add_extra_path(_extra_paths CMAKE_PREFIX_PATH)
      _pkgconfig_add_extra_path(_extra_paths CMAKE_FRAMEWORK_PATH)
      _pkgconfig_add_extra_path(_extra_paths CMAKE_APPBUNDLE_PATH)
    endif()

    if(NOT _no_cmake_environment_path)
      _pkgconfig_add_extra_path(_extra_paths ENV CMAKE_PREFIX_PATH)
      _pkgconfig_add_extra_path(_extra_paths ENV CMAKE_FRAMEWORK_PATH)
      _pkgconfig_add_extra_path(_extra_paths ENV CMAKE_APPBUNDLE_PATH)
    endif()

    if(NOT "${_extra_paths}" STREQUAL "")
      # Save the PKG_CONFIG_PATH environment variable, and add paths
      # from the CMAKE_PREFIX_PATH variables
      set(_pkgconfig_path_old $ENV{PKG_CONFIG_PATH})
      set(_pkgconfig_path ${_pkgconfig_path_old})
      if(NOT "${_pkgconfig_path}" STREQUAL "")
        file(TO_CMAKE_PATH "${_pkgconfig_path}" _pkgconfig_path)
      endif()

      # Create a list of the possible pkgconfig subfolder (depending on
      # the system
      set(_lib_dirs)
      if(NOT DEFINED CMAKE_SYSTEM_NAME
          OR (CMAKE_SYSTEM_NAME MATCHES "^(Linux|kFreeBSD|GNU)$"
              AND NOT CMAKE_CROSSCOMPILING))
        if(EXISTS "/etc/debian_version") # is this a debian system ?
          if(CMAKE_LIBRARY_ARCHITECTURE)
            list(APPEND _lib_dirs "lib/${CMAKE_LIBRARY_ARCHITECTURE}/pkgconfig")
          endif()
        else()
          # not debian, chech the FIND_LIBRARY_USE_LIB64_PATHS property
          get_property(uselib64 GLOBAL PROPERTY FIND_LIBRARY_USE_LIB64_PATHS)
          if(uselib64)
            list(APPEND _lib_dirs "lib64/pkgconfig")
          endif()
        endif()
      endif()
      list(APPEND _lib_dirs "lib/pkgconfig")

      # Check if directories exist and eventually append them to the
      # pkgconfig path list
      foreach(_prefix_dir ${_extra_paths})
        foreach(_lib_dir ${_lib_dirs})
          if(EXISTS "${_prefix_dir}/${_lib_dir}")
            list(APPEND _pkgconfig_path "${_prefix_dir}/${_lib_dir}")
            list(REMOVE_DUPLICATES _pkgconfig_path)
          endif()
        endforeach()
      endforeach()

      # Prepare and set the environment variable
      if(NOT "${_pkgconfig_path}" STREQUAL "")
        # remove empty values from the list
        list(REMOVE_ITEM _pkgconfig_path "")
        file(TO_NATIVE_PATH "${_pkgconfig_path}" _pkgconfig_path)
        if(UNIX)
          string(REPLACE ";" ":" _pkgconfig_path "${_pkgconfig_path}")
          string(REPLACE "\\ " " " _pkgconfig_path "${_pkgconfig_path}")
        endif()
        set(ENV{PKG_CONFIG_PATH} "${_pkgconfig_path}")
      endif()

      # Unset variables
      unset(_lib_dirs)
      unset(_pkgconfig_path)
    endif()

    # iterate through module list and check whether they exist and match the required version
    foreach (_pkg_check_modules_pkg ${_pkg_check_modules_list})
      set(_pkg_check_modules_exist_query)

      # check whether version is given
      if (_pkg_check_modules_pkg MATCHES "(.*[^><])(>=|=|<=)(.*)")
        set(_pkg_check_modules_pkg_name "${CMAKE_MATCH_1}")
        set(_pkg_check_modules_pkg_op "${CMAKE_MATCH_2}")
        set(_pkg_check_modules_pkg_ver "${CMAKE_MATCH_3}")
      else()
        set(_pkg_check_modules_pkg_name "${_pkg_check_modules_pkg}")
        set(_pkg_check_modules_pkg_op)
        set(_pkg_check_modules_pkg_ver)
      endif()

      # handle the operands
      if (_pkg_check_modules_pkg_op STREQUAL ">=")
        list(APPEND _pkg_check_modules_exist_query --atleast-version)
      endif()

      if (_pkg_check_modules_pkg_op STREQUAL "=")
        list(APPEND _pkg_check_modules_exist_query --exact-version)
      endif()

      if (_pkg_check_modules_pkg_op STREQUAL "<=")
        list(APPEND _pkg_check_modules_exist_query --max-version)
      endif()

      # create the final query which is of the format:
      # * --atleast-version <version> <pkg-name>
      # * --exact-version <version> <pkg-name>
      # * --max-version <version> <pkg-name>
      # * --exists <pkg-name>
      if (_pkg_check_modules_pkg_op)
        list(APPEND _pkg_check_modules_exist_query "${_pkg_check_modules_pkg_ver}")
      else()
        list(APPEND _pkg_check_modules_exist_query --exists)
      endif()

      _pkgconfig_unset(${_prefix}_${_pkg_check_modules_pkg_name}_VERSION)
      _pkgconfig_unset(${_prefix}_${_pkg_check_modules_pkg_name}_PREFIX)
      _pkgconfig_unset(${_prefix}_${_pkg_check_modules_pkg_name}_INCLUDEDIR)
      _pkgconfig_unset(${_prefix}_${_pkg_check_modules_pkg_name}_LIBDIR)

      list(APPEND _pkg_check_modules_exist_query "${_pkg_check_modules_pkg_name}")
      list(APPEND _pkg_check_modules_packages    "${_pkg_check_modules_pkg_name}")

      # execute the query
      execute_process(
        COMMAND ${PKG_CONFIG_EXECUTABLE} ${_pkg_check_modules_exist_query}
        RESULT_VARIABLE _pkgconfig_retval)

      # evaluate result and tell failures
      if (_pkgconfig_retval)
        if(NOT ${_is_silent})
          message(STATUS "  Package '${_pkg_check_modules_pkg}' not found")
        endif()

        set(_pkg_check_modules_failed 1)
      endif()
    endforeach()

    if(_pkg_check_modules_failed)
      # fail when requested
      if (${_is_required})
        message(FATAL_ERROR "A required package was not found")
      endif ()
    else()
      # when we are here, we checked whether requested modules
      # exist. Now, go through them and set variables

      _pkgconfig_set(${_prefix}_FOUND 1)
      list(LENGTH _pkg_check_modules_packages pkg_count)

      # iterate through all modules again and set individual variables
      foreach (_pkg_check_modules_pkg ${_pkg_check_modules_packages})
        # handle case when there is only one package required
        if (pkg_count EQUAL 1)
          set(_pkg_check_prefix "${_prefix}")
        else()
          set(_pkg_check_prefix "${_prefix}_${_pkg_check_modules_pkg}")
        endif()

        _pkgconfig_invoke(${_pkg_check_modules_pkg} "${_pkg_check_prefix}" VERSION    ""   --modversion )
        pkg_get_variable("${_pkg_check_prefix}_PREFIX" ${_pkg_check_modules_pkg} "prefix")
        pkg_get_variable("${_pkg_check_prefix}_INCLUDEDIR" ${_pkg_check_modules_pkg} "includedir")
        pkg_get_variable("${_pkg_check_prefix}_LIBDIR" ${_pkg_check_modules_pkg} "libdir")

        if (NOT ${_is_silent})
          message(STATUS "  Found ${_pkg_check_modules_pkg}, version ${_pkgconfig_VERSION}")
        endif ()
      endforeach()

      # set variables which are combined for multiple modules
      _pkgconfig_invoke_dyn("${_pkg_check_modules_packages}" "${_prefix}" LIBRARIES           "(^| )-l" --libs-only-l )
      _pkgconfig_invoke_dyn("${_pkg_check_modules_packages}" "${_prefix}" LIBRARY_DIRS        "(^| )-L" --libs-only-L )
      _pkgconfig_invoke_dyn("${_pkg_check_modules_packages}" "${_prefix}" LDFLAGS             ""        --libs )
      _pkgconfig_invoke_dyn("${_pkg_check_modules_packages}" "${_prefix}" LDFLAGS_OTHER       ""        --libs-only-other )

      _pkgconfig_invoke_dyn("${_pkg_check_modules_packages}" "${_prefix}" INCLUDE_DIRS        "(^| )-I" --cflags-only-I )
      _pkgconfig_invoke_dyn("${_pkg_check_modules_packages}" "${_prefix}" CFLAGS              ""        --cflags )
      _pkgconfig_invoke_dyn("${_pkg_check_modules_packages}" "${_prefix}" CFLAGS_OTHER        ""        --cflags-only-other )
    endif()

    if(NOT "${_extra_paths}" STREQUAL "")
      # Restore the environment variable
      set(ENV{PKG_CONFIG_PATH} ${_pkgconfig_path_old})
    endif()

    unset(_extra_paths)
    unset(_pkgconfig_path_old)
  else()
    if (${_is_required})
      message(SEND_ERROR "pkg-config tool not found")
    endif ()
  endif()
endmacro()

macro(gnc_pkg_check_modules _prefix _module0)
  # check cached value
  if (NOT DEFINED __pkg_config_checked_${_prefix} OR __pkg_config_checked_${_prefix} LESS ${PKG_CONFIG_VERSION} OR NOT ${_prefix}_FOUND)
    _pkgconfig_parse_options   (_pkg_modules _pkg_is_required _pkg_is_silent _no_cmake_path _no_cmake_environment_path "${_module0}" ${ARGN})
    _gnc_pkg_check_modules_internal("${_pkg_is_required}" "${_pkg_is_silent}" ${_no_cmake_path} ${_no_cmake_environment_path} "${_prefix}" ${_pkg_modules})
    _pkgconfig_set(__pkg_config_checked_${_prefix} ${PKG_CONFIG_VERSION})
  endif()
endmacro()

else()

include(FindPkgConfig)

macro(gnc_pkg_check_modules _prefix _module0)
   PKG_CHECK_MODULES(${_prefix} ${_module0} ${ARGN})
endmacro()

endif()
