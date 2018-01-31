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


if (GENERATE_SWIG_WRAPPERS)
    set (SWIG_RUNTIME_H ${CMAKE_CURRENT_BINARY_DIR}/swig-runtime.h)
    # Command to generate the swig-runtime.h header
    add_custom_command (
        OUTPUT ${SWIG_RUNTIME_H}
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/CMakeLists.txt
        COMMAND ${SWIG_EXECUTABLE} -guile -external-runtime ${SWIG_RUNTIME_H}
    )
else()
    set (SWIG_RUNTIME_H ${CMAKE_CURRENT_SOURCE_DIR}/swig-runtime.h)
endif()
dist_add_generated (${GENERATE_SWIG_WRAPPERS} swig-runtime.h)

add_custom_target (swig-runtime-h DEPENDS ${SWIG_RUNTIME_H})

SET_LOCAL_DIST(common_DIST_local CMakeLists.txt ${common_EXTRA_DIST})

SET(common_DIST ${common_DIST_local} ${cmake_modules_DIST} ${debug_DIST} ${test_core_DIST} PARENT_SCOPE)
