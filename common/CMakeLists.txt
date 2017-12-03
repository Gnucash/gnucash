# CMakeLists.txt for common/

# The subdirectories
ADD_SUBDIRECTORY (cmake_modules)
ADD_SUBDIRECTORY (debug)
ADD_SUBDIRECTORY (test-core)

SET(common_EXTRA_DIST
        base-typemaps.i
        config.h.cmake.in
        gnc-test-env.pl
        guile-mappings.h
        platform.h)

IF (BUILDING_FROM_VCS)
  SET (SWIG_RUNTIME_H ${CMAKE_CURRENT_BINARY_DIR}/swig-runtime.h PARENT_SCOPE)
ELSE()
  SET (SWIG_RUNTIME_H ${CMAKE_CURRENT_SOURCE_DIR}/swig-runtime.h PARENT_SCOPE)
ENDIF()


SET_LOCAL_DIST(common_DIST_local CMakeLists.txt ${common_EXTRA_DIST})

SET(common_DIST ${common_DIST_local} ${cmake_modules_DIST} ${debug_DIST} ${test_core_DIST} PARENT_SCOPE)
