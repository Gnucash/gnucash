
function(set_local_dist output)
    set(dist_files "")
    foreach(file ${ARGN})
        file(RELATIVE_PATH relative ${CMAKE_SOURCE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/${file})
        list(APPEND dist_files ${relative})
    endforeach()
    set (${output} ${dist_files} PARENT_SCOPE)
endfunction()

macro(set_dist_list output)
    set_local_dist(${output}_TMP ${ARGN})
    set(${output} ${${output}_TMP} PARENT_SCOPE)
endmacro()

function(execute_process_and_check_result)
    cmake_parse_arguments(VARS "" "WORKING_DIRECTORY;ERROR_MSG" "COMMAND" ${ARGN})
    execute_process(
            COMMAND ${VARS_COMMAND}
            WORKING_DIRECTORY ${VARS_WORKING_DIRECTORY}
            RESULT_VARIABLE RESULT
    )
    if (NOT "${RESULT}" STREQUAL "0")
        message(FATAL_ERROR ${VARS_ERROR_MSG})
    endif()
endfunction()

# These functions can be called to add generated files (as opposed to source files)
# to the distribution tarball
# - dist_add_configured will only add the files to the tarball. This function
#   should be used for files that are generated during a cmake run (for example with configure_file)
# - dist_add_generated will add the files to the tarball and generate a dependency
#   for each file to the dist target. This function should be used for all files that
#   will be generated during a "make" or "ninja-build" run (for example those that are
#   the result of an add_custom_command)
#
# Note that the files passed in these functions can still be in the source directory
# This depends on whether or not the source is a git worktree or not and which
# options were passed to cmake.
# To determine this, the first parameter passed to this function should
# be an indicator whether or not the files in the list are actually built
# in this configuration. At the time of this writing there are two
# cmake variables that control this, and hence can be used here:
# BUILDING_FROM_VCS: is YES when building from a git source tree
# GENERATE_SWIG_WRAPPERS: is YES when the swig wrappers should be built
function (dist_add_generated_internal _needs_target _is_built)
    if (_is_built)
        set (DIST_BUILD_SOURCE_DIR ${CMAKE_BINARY_DIR})
        set (DIST_CURRENT_BUILD_SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR})
    else()
        set (DIST_BUILD_SOURCE_DIR ${CMAKE_SOURCE_DIR})
        set (DIST_CURRENT_BUILD_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR})
    endif()

    set (local_generated ${dist_generated})
    foreach (conf_file ${ARGN})
        file (RELATIVE_PATH rel_conf ${DIST_BUILD_SOURCE_DIR} ${DIST_CURRENT_BUILD_SOURCE_DIR}/${conf_file})
        list (APPEND local_generated ${rel_conf})
    endforeach()
    set (dist_generated ${local_generated}
        CACHE INTERNAL "generated files that will be included in the distribution tarball")

    if (_needs_target)
        set (local_generated_depends ${dist_generated_depends})
        foreach (gen_file ${ARGN})
            file (RELATIVE_PATH rel_file ${DIST_BUILD_SOURCE_DIR} ${DIST_CURRENT_BUILD_SOURCE_DIR}/${gen_file})
            string (REPLACE "/" "-" target_tmp ${rel_file})
            string (REPLACE "_" "-" target_tmp2 ${target_tmp})
            string (REPLACE "." "-" dist_target dist-${target_tmp2})

            add_custom_target (${dist_target}
                DEPENDS ${DIST_CURRENT_BUILD_SOURCE_DIR}/${gen_file})
            list (APPEND local_generated_depends ${dist_target})
        endforeach()
        set (dist_generated_depends ${local_generated_depends}
            CACHE INTERNAL "global targets for generated files that will be included in the distribution tarball")
    endif()
endfunction()

function (dist_add_configured _is_built)
    dist_add_generated_internal (NO ${_is_built} ${ARGN})
endfunction()

function (dist_add_generated _is_built)
    dist_add_generated_internal (YES ${_is_built} ${ARGN})
endfunction()
