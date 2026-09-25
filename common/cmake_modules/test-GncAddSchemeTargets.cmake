# Verify that Guile install directories stay below CMAKE_INSTALL_PREFIX.

if(TEST_MINGW)
  set(MINGW TRUE)
endif()
include("${TEST_MODULE}")

make_guile_relative_path(_site_dir
  "${TEST_GUILE_SITEDIR}" "${TEST_GUILE_PREFIX}")
make_guile_relative_path(_site_cache_dir
  "${TEST_GUILE_SITECCACHEDIR}" "${TEST_GUILE_PREFIX}")

if(NOT "${_site_dir}" STREQUAL "${TEST_GUILE_REL_SITEDIR}")
  message(FATAL_ERROR
    "Unexpected Guile site directory: '${_site_dir}', expected '${TEST_GUILE_REL_SITEDIR}'")
endif()
if(NOT "${_site_cache_dir}" STREQUAL "${TEST_GUILE_REL_SITECCACHEDIR}")
  message(FATAL_ERROR
    "Unexpected Guile site cache directory: '${_site_cache_dir}', expected '${TEST_GUILE_REL_SITECCACHEDIR}'")
endif()

foreach(_dir IN ITEMS
    "${TEST_GUILE_REL_DATADIR}"
    "${TEST_GUILE_REL_LIBDIR}"
    "${TEST_GUILE_REL_CCACHEDIR}"
    "${TEST_GUILE_REL_SITEDIR}"
    "${TEST_GUILE_REL_SITECCACHEDIR}")
  if(IS_ABSOLUTE "${_dir}" OR _dir MATCHES "^[A-Za-z]:[/\\\\]" OR
      _dir MATCHES "^\\.\\.([/\\\\]|$)")
    message(FATAL_ERROR "Guile install directory is not prefix-relative: ${_dir}")
  endif()
endforeach()
