if(NOT DIST_MANIFEST)
  message(FATAL_ERROR "DIST_MANIFEST is required")
endif()

if(NOT EXISTS "${DIST_MANIFEST}")
  message(FATAL_ERROR "Dist manifest not found: ${DIST_MANIFEST}")
endif()

file(STRINGS "${DIST_MANIFEST}" dist_files)
set(required_cmake_files
  cmake/check-cmake-dist-manifest.cmake
  cmake/check-gtk-window-application-bindings.cmake
  cmake/check-no-local-main-loop-pumps.cmake
  cmake/run-ctest.cmake)

foreach(required_file IN LISTS required_cmake_files)
  list(FIND dist_files "${required_file}" required_file_index)
  if(required_file_index EQUAL -1)
    message(FATAL_ERROR "${required_file} is missing from the source dist")
  endif()
endforeach()
