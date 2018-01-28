# Copyright (c) 2010, Christian Stimming


# Redistribution and use is allowed according to the terms of the BSD license.
# For details see the accompanying COPYING-CMAKE-SCRIPTS file.

macro (GNC_ADD_SWIG_COMMAND _target _output _input)

    add_custom_command (
        OUTPUT ${_output}
        DEPENDS ${_input} ${CMAKE_SOURCE_DIR}/common/base-typemaps.i ${ARGN}
        COMMAND ${SWIG_EXECUTABLE} -guile ${SWIG_ARGS} -Linkage module -I${CMAKE_SOURCE_DIR}/libgnucash/engine -I${CMAKE_SOURCE_DIR}/common  -o ${_output} ${_input}
    )

    add_custom_target(${_target} DEPENDS ${_output})

    if (BUILDING_FROM_VCS)
        set(BUILD_SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR})
    else()
        set(BUILD_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR})
    endif()

    file(RELATIVE_PATH generated ${BUILD_SOURCE_DIR} ${_output})
    dist_add_generated(${_target} ${generated})
endmacro (GNC_ADD_SWIG_COMMAND)


macro (GNC_ADD_SWIG_PYTHON_COMMAND _target _output _input)

    set (DEFAULT_SWIG_PYTHON_FLAGS
        -python
        -Wall -Werror
        ${SWIG_ARGS}
    )
    set (DEFAULT_SWIG_PYTHON_C_INCLUDES
        ${GLIB2_INCLUDE_DIRS}
        ${CMAKE_SOURCE_DIR}/common
        ${CMAKE_SOURCE_DIR}/libgnucash/engine
        ${CMAKE_SOURCE_DIR}/libgnucash/app-utils
    )


    set (PYTHON_SWIG_FLAGS ${DEFAULT_SWIG_PYTHON_FLAGS})
    foreach (dir ${DEFAULT_SWIG_PYTHON_C_INCLUDES})
        list (APPEND PYTHON_SWIG_FLAGS "-I${dir}")
    endforeach (dir)
    add_custom_command(OUTPUT ${_output}
        COMMAND ${SWIG_EXECUTABLE} ${PYTHON_SWIG_FLAGS} -o ${_output} ${_input}
        DEPENDS ${_input} ${CMAKE_SOURCE_DIR}/common/base-typemaps.i ${ARGN}
    )
    add_custom_target(${_target} ALL DEPENDS ${_output} ${CMAKE_SOURCE_DIR}/common/base-typemaps.i ${_input} ${ARGN})


    if (BUILDING_FROM_VCS)
        set(BUILD_SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR})
    else()
        set(BUILD_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR})
    endif()

    file(RELATIVE_PATH generated ${BUILD_SOURCE_DIR} ${_output})
    dist_add_generated(${_target} ${generated})
endmacro()
