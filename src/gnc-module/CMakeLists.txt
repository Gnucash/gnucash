# CMakeLists.txt for src/gnc-module

ADD_DEFINITIONS (-DG_LOG_DOMAIN=\"gnc.module\")

INCLUDE_DIRECTORIES (${GLIB2_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${LIBINTL_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${REGEX_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${LIBGUILE_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${CMAKE_BINARY_DIR} ) # for config.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}) # for gnc-ui.h
INCLUDE_DIRECTORIES (${CMAKE_CURRENT_SOURCE_DIR}) # when building swig-gnc-module.c

# Command to generate the swig-engine.c wrapper file
SET (SWIG_GNC_MODULE_C ${CMAKE_CURRENT_BINARY_DIR}/swig-gnc-module.c)
GNC_ADD_SWIG_COMMAND (${SWIG_GNC_MODULE_C} ${CMAKE_CURRENT_SOURCE_DIR}/gnc-module.i)

SET (libgnc_module_SOURCES
  gnc-module.c
  ${SWIG_GNC_MODULE_C}
)

SET (libgnc_module_HEADERS
  gnc-module.h
  gnc-module-api.h
)

ADD_LIBRARY	(gnc-module
  ${libgnc_module_SOURCES}
  ${libgnc_module_HEADERS}
  )
