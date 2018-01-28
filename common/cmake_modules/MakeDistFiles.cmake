
IF (${CMAKE_VERSION} VERSION_LESS 3.3)
    INCLUDE(CMakeParseArguments)
ENDIF()


FUNCTION(SET_LOCAL_DIST output)
    SET(dist_files "")
    FOREACH(file ${ARGN})
        FILE(RELATIVE_PATH relative ${CMAKE_SOURCE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/${file})
        LIST(APPEND dist_files ${relative})
    ENDFOREACH()
    SET (${output} ${dist_files} PARENT_SCOPE)
ENDFUNCTION()

MACRO(SET_DIST_LIST output)
    SET_LOCAL_DIST(${output}_TMP ${ARGN})
    SET(${output} ${${output}_TMP} PARENT_SCOPE)
ENDMACRO()

FUNCTION(EXECUTE_PROCESS_AND_CHECK_RESULT)
    cmake_parse_arguments(VARS "" "WORKING_DIRECTORY;ERROR_MSG" "COMMAND" ${ARGN})
    EXECUTE_PROCESS(
            COMMAND ${VARS_COMMAND}
            WORKING_DIRECTORY ${VARS_WORKING_DIRECTORY}
            RESULT_VARIABLE RESULT
    )
    IF (NOT "${RESULT}" STREQUAL "0")
        MESSAGE(FATAL_ERROR ${VARS_ERROR_MSG})
    ENDIF()
ENDFUNCTION()

# These macros can be called to add a generated file (as opposed to a source file)
# to the distribution tarball
# - dist_add_configured will only add the file to the tarball. This macro
#   should be used for files that are generated during a cmake run (for example with configure_file)
# - dist_add_generated will add the file to the tarball and generate a dependency
#   for the file to the dist target. This macro should be used for all files that
#   will be generated during a "make" or "ninja-build" run
macro(dist_add_configured _configured)
    if (BUILDING_FROM_VCS)
        set(BUILD_SOURCE_DIR ${CMAKE_BINARY_DIR})
        set(CURRENT_BUILD_SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR})
    else()
        set(BUILD_SOURCE_DIR ${CMAKE_SOURCE_DIR})
        set(CURRENT_BUILD_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR})
    endif()
    file(RELATIVE_PATH rel_conf ${BUILD_SOURCE_DIR} ${CURRENT_BUILD_SOURCE_DIR}/${_configured})

    set(local_generated ${dist_generated})
    list(APPEND local_generated ${rel_conf})
    set(dist_generated ${local_generated} CACHE INTERNAL "generated files that will be included in the distribution tarball")
endmacro()

macro(dist_add_generated _target _generated)
    dist_add_configured(${_generated})

    set(local_generated_depends ${dist_generated_depends})
    list(APPEND local_generated_depends ${_target})
    set(dist_generated_depends ${local_generated_depends} CACHE INTERNAL "global targets for generated files that will be included in the distribution tarball")
endmacro()
