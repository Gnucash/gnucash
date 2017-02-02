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

  ADD_CUSTOM_COMMAND(OUTPUT ${_output}

    COMMAND ${SWIG_EXECUTABLE} -python -Wall -Werror ${SWIG_ARGS}
       -I${CMAKE_SOURCE_DIR}/src/libqof/qof -I${CMAKE_SOURCE_DIR}/src
       -I${CMAKE_SOURCE_DIR}/src/engine -I${CMAKE_SOURCE_DIR}/src/app-utils
       -o ${_output} ${_input}
    DEPENDS ${_input} ${CMAKE_SOURCE_DIR}/src/base-typemaps.i ${ARGN}
  )
  ADD_CUSTOM_TARGET(${_target} ALL DEPENDS ${_output} ${CMAKE_SOURCE_DIR}/src/base-typemaps.i ${_input} ${ARGN})
ENDMACRO()
