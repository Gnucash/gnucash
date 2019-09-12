#Pinched from FindPkgConfig in CMake 3.6 because we need to do this in
#CMake 3.5. Delete this file and the corresponding calls in master
#after merging up.

# given directories, and create an imported target from them
function(_pkg_create_imp_target _prefix)
  unset(_libs)
  unset(_find_opts)

  # set the options that are used as long as the .pc file does not provide a library
  # path to look into

  foreach (flag IN LISTS ${_prefix}_LDFLAGS)
    if (flag MATCHES "^-L(.*)")
      # only look into the given paths from now on
      set(_find_opts "HINTS ${${CMAKE_MATCH_1}} NO_DEFAULT_PATH")
      continue()
    endif()
    if (flag MATCHES "^-l(.*)")
      set(_pkg_search "${CMAKE_MATCH_1}")
    else()
      continue()
    endif()

    find_library(pkgcfg_lib_${_prefix}_${_pkg_search}
                 NAMES ${_pkg_search}
                 ${_find_opts})
    list(APPEND _libs "${pkgcfg_lib_${_prefix}_${_pkg_search}}")
  endforeach()

  # only create the target if it is linkable, i.e. no executables
  if (NOT TARGET PkgConfig::${_prefix}
      AND ( ${_prefix}_INCLUDE_DIRS OR _libs OR ${_prefix}_CFLAGS_OTHER ))
    add_library(PkgConfig::${_prefix} INTERFACE IMPORTED)

    unset(_props)
    if(${_prefix}_INCLUDE_DIRS)
      set_property(TARGET PkgConfig::${_prefix} PROPERTY
                   INTERFACE_INCLUDE_DIRECTORIES "${${_prefix}_INCLUDE_DIRS}")
    endif()
    if(_libs)
      set_property(TARGET PkgConfig::${_prefix} PROPERTY
                   INTERFACE_LINK_LIBRARIES "${_libs}")
    endif()
    if(${_prefix}_CFLAGS_OTHER)
      set_property(TARGET PkgConfig::${_prefix} PROPERTY
                   INTERFACE_COMPILE_OPTIONS "${${_prefix}_CFLAGS_OTHER}")
    endif()
  endif()
endfunction()


