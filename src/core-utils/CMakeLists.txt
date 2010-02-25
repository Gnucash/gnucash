# CMakeLists.txt for src/core-utils

ADD_DEFINITIONS (-DG_LOG_DOMAIN=\"gnc.core-utils\")

INCLUDE_DIRECTORIES (${GLIB2_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${GCONF2_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${LIBINTL_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${REGEX_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${LIBGUILE_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${CMAKE_BINARY_DIR} ) # for config.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}) # for gnc-ui.h
INCLUDE_DIRECTORIES (${CMAKE_CURRENT_SOURCE_DIR}) # when building swig-core-utils.c

# Command to generate the swig-engine.c wrapper file
SET (SWIG_CORE_UTILS_C ${CMAKE_CURRENT_BINARY_DIR}/swig-core-utils.c)
GNC_ADD_SWIG_COMMAND (${SWIG_CORE_UTILS_C} ${CMAKE_CURRENT_SOURCE_DIR}/core-utils.i)

SET (libgnc_core_utils_SOURCES
  gnc-main.c
  gnc-gconf-utils.c
  gnc-gdate-utils.c
  gnc-gkeyfile-utils.c
  gnc-glib-utils.c
  ${SWIG_CORE_UTILS_C}
)

SET (libgnc_core_utils_HEADERS
  gnc-main.h
  gnc-gconf-utils.h
  gnc-gdate-utils.h
  gnc-gkeyfile-utils.h
  gnc-glib-utils.h
)

ADD_LIBRARY	(core-utils
  ${libgnc_core_utils_SOURCES}
  ${libgnc_core_utils_HEADERS}
  )
