# Copyright (c) 2010, Christian Stimming


# Redistribution and use is allowed according to the terms of the BSD license.
# For details see the accompanying COPYING-CMAKE-SCRIPTS file.

MACRO (GNC_ADD_SWIG_COMMAND _target _input)

ADD_CUSTOM_COMMAND (
  OUTPUT ${_target}
  DEPENDS ${_input} ${CMAKE_SOURCE_DIR}/base-typemaps.i ${ARGN}
  COMMAND ${SWIG_EXECUTABLE} -guile ${SWIG_ARGS} -Linkage module -I${CMAKE_SOURCE_DIR}/libqof/qof -I${CMAKE_SOURCE_DIR}  -o ${_target} ${_input}
)

ENDMACRO (GNC_ADD_SWIG_COMMAND)
