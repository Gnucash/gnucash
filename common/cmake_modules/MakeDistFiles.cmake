
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

macro(make_target_name target source)
    if(BUILDING_FROM_VCS)
        set(TGT_BUILD_SOURCE_DIR ${CMAKE_BINARY_DIR})
        set(TGT_CURRENT_BUILD_SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR})
    else()
        set(TGT_BUILD_SOURCE_DIR ${CMAKE_SOURCE_DIR})
        set(TGT_CURRENT_BUILD_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR})
    endif()

    file(RELATIVE_PATH rel_file ${TGT_BUILD_SOURCE_DIR} ${TGT_CURRENT_BUILD_SOURCE_DIR}/${source})
    string(REPLACE "/" "-" target_tmp ${rel_file})
    string(REPLACE "_" "-" target_tmp2 ${target_tmp})
    string(REPLACE "." "-" ${target} dist-${target_tmp2})
endmacro()

# These macros can be called to add generated files (as opposed to source files)
# to the distribution tarball
# - dist_add_configured will only add the files to the tarball. This macro
#   should be used for files that are generated during a cmake run (for example with configure_file)
# - dist_add_generated will add the files to the tarball and generate a dependency
#   for each file to the dist target. This macro should be used for all files that
#   will be generated during a "make" or "ninja-build" run (for example those that are
#   the result of an add_custom_command)
macro(dist_add_configured)
    if(BUILDING_FROM_VCS)
        set(CONF_BUILD_SOURCE_DIR ${CMAKE_BINARY_DIR})
        set(CONF_CURRENT_BUILD_SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR})
    else()
        set(CONF_BUILD_SOURCE_DIR ${CMAKE_SOURCE_DIR})
        set(CONF_CURRENT_BUILD_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR})
    endif()

    set(local_generated ${dist_generated})
    foreach(conf_file ${ARGN})
        file(RELATIVE_PATH rel_conf ${CONF_BUILD_SOURCE_DIR} ${CONF_CURRENT_BUILD_SOURCE_DIR}/${conf_file})
        list(APPEND local_generated ${rel_conf})
    endforeach()
    set(dist_generated ${local_generated} CACHE INTERNAL "generated files that will be included in the distribution tarball")
endmacro()

macro(dist_add_generated)
    dist_add_configured(${ARGN})

    set(local_generated_depends ${dist_generated_depends})
    foreach(gen_file ${ARGN})
        make_target_name(dist_target ${gen_file})
        add_custom_target(${dist_target} DEPENDS ${TGT_CURRENT_BUILD_SOURCE_DIR}/${gen_file}) # Note TGT_CURRENT_BUILD_SOURCE_DIR trickles down from the make_target macro
        list(APPEND local_generated_depends ${dist_target})
    endforeach()
    set(dist_generated_depends ${local_generated_depends} CACHE INTERNAL "global targets for generated files that will be included in the distribution tarball")
endmacro()
