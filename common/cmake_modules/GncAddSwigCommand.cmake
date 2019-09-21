# Copyright (c) 2010, Christian Stimming
# Copyright (c) 2018, Geert Janssens

# Redistribution and use is allowed according to the terms of the BSD license.
# For details see the accompanying COPYING-CMAKE-SCRIPTS file.

# gnc_add_swig_guile_command is used to generate guile swig wrappers
# - _target is the name of a global target that will be set for this wrapper file,
#    this can be used elsewhere to create a depencency on this wrapper
# - _out_var will be set to the full path to the generated wrapper file
# - _output is the name of the wrapper file to generate
# - _input is the swig interface file (*.i) to generate this wrapper from
# Any additional parameters will be used as dependencies for this wrapper target
macro (gnc_add_swig_guile_command _target _out_var _output _input)

    set(SW_CURR_BUILD_SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR})
    set(SW_BUILD_SOURCE_DIR ${CMAKE_BINARY_DIR})
    set(outfile ${SW_CURR_BUILD_SOURCE_DIR}/${_output})
    set(${_out_var} ${outfile}) # This variable is set for convenience to use in the calling CMakeLists.txt

    add_custom_command (
        OUTPUT ${outfile}
        DEPENDS ${_input} ${CMAKE_SOURCE_DIR}/common/base-typemaps.i ${ARGN}
        COMMAND ${SWIG_EXECUTABLE} -guile ${SWIG_ARGS} -Linkage module -I${CMAKE_SOURCE_DIR}/libgnucash/engine -I${CMAKE_SOURCE_DIR}/common  -o ${outfile} ${_input}
    )
    add_custom_target(${_target} DEPENDS ${outfile})
endmacro (gnc_add_swig_guile_command)


# gnc_add_swig_python_command is used to generate python swig wrappers
# from the tarball will be used instead
# - _target is the name of a global target that will be set for this wrapper file,
#    this can be used elsewhere to create a depencency on this wrapper
# - _out_var will be set to the full path to the generated wrapper file
# - _py_out_var is the same but for the python module that's generated together with the wrapper
# - _output is the name of the wrapper file to generate
# - _py_output is the name of the python module associated with this wrapper
# - _input is the swig interface file (*.i) to generate this wrapper from
# Any additional parameters will be used as dependencies for this wrapper target
macro (gnc_add_swig_python_command _target _out_var _py_out_var _output _py_output _input)

    set(SW_CURR_BUILD_SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR})
    set(SW_BUILD_SOURCE_DIR ${CMAKE_BINARY_DIR})
    set(outfile ${SW_CURR_BUILD_SOURCE_DIR}/${_output})
    set(${_out_var} ${outfile}) # This variable is set for convenience to use in the calling CMakeLists.txt

    set(py_outfile ${SW_CURR_BUILD_SOURCE_DIR}/${_py_output})
    set(${_py_out_var} ${py_outfile}) # This variable is set for convenience to use in the calling CMakeLists.txt

    set (DEFAULT_SWIG_PYTHON_FLAGS
        -python -py3
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
    add_custom_command(OUTPUT ${outfile} ${py_outfile}
        COMMAND ${SWIG_EXECUTABLE} ${PYTHON_SWIG_FLAGS} -o ${outfile} ${_input}
        DEPENDS ${_input} ${CMAKE_SOURCE_DIR}/common/base-typemaps.i ${ARGN}
    )
    add_custom_target(${_target} ALL DEPENDS ${outfile} ${py_outfile} ${CMAKE_SOURCE_DIR}/common/base-typemaps.i ${_input} ${ARGN})
endmacro()
