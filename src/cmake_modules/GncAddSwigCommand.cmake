# Copyright (c) 2010, Christian Stimming


# Redistribution and use is allowed according to the terms of the BSD license.
# For details see the accompanying COPYING-CMAKE-SCRIPTS file.

MACRO (GNC_ADD_SWIG_COMMAND _target _output _input)

ADD_CUSTOM_COMMAND (
  OUTPUT ${_output}
  DEPENDS ${_input} ${CMAKE_SOURCE_DIR}/src/base-typemaps.i ${ARGN}
COMMAND ${SWIG_EXECUTABLE} -guile ${SWIG_ARGS} -Linkage module -I${CMAKE_SOURCE_DIR}/src/libqof/qof -I${CMAKE_SOURCE_DIR}/src  -o ${_output} ${_input}
)

ADD_CUSTOM_TARGET(${_target} DEPENDS ${_output})

ENDMACRO (GNC_ADD_SWIG_COMMAND)


MACRO (GNC_ADD_SWIG_PYTHON_COMMAND _target _output _input)

  set (DEFAULT_SWIG_PYTHON_FLAGS
    -python
    -Wall -Werror
    ${SWIG_ARGS}
  )
  set (DEFAULT_SWIG_PYTHON_C_INCLUDES
    ${GLIB2_INCLUDE_DIRS}
    ${CMAKE_SOURCE_DIR}/src/libqof/qof
    ${CMAKE_SOURCE_DIR}/src
    ${CMAKE_SOURCE_DIR}/src/engine
    ${CMAKE_SOURCE_DIR}/src/app-utils
  )


  set (PYTHON_SWIG_FLAGS ${DEFAULT_SWIG_PYTHON_FLAGS})
  foreach (dir ${DEFAULT_SWIG_PYTHON_C_INCLUDES})
    list (APPEND PYTHON_SWIG_FLAGS "-I${dir}")
  endforeach (dir)
  ADD_CUSTOM_COMMAND(OUTPUT ${_output}
    COMMAND ${SWIG_EXECUTABLE} ${PYTHON_SWIG_FLAGS} -o ${_output} ${_input}
    DEPENDS ${_input} ${CMAKE_SOURCE_DIR}/src/base-typemaps.i ${ARGN}
  )
  ADD_CUSTOM_TARGET(${_target} ALL DEPENDS ${_output} ${CMAKE_SOURCE_DIR}/src/base-typemaps.i ${_input} ${ARGN})

ENDMACRO(GNC_ADD_SWIG_PYTHON_COMMAND)
