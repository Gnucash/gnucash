# CMakeLists.txt for src/core-utils

ADD_DEFINITIONS (-DG_LOG_DOMAIN=\"gnc.core-utils\")

INCLUDE_DIRECTORIES (${GLIB2_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${GCONF2_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${LIBINTL_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${REGEX_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${LIBGUILE_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${CMAKE_BINARY_DIR}/src ) # for config.h
INCLUDE_DIRECTORIES (${CMAKE_CURRENT_SOURCE_DIR}/..) # for gnc-ui.h
INCLUDE_DIRECTORIES (${CMAKE_CURRENT_SOURCE_DIR}/../libqof/qof) # for gnc-ui.h
INCLUDE_DIRECTORIES (${CMAKE_CURRENT_SOURCE_DIR}) # when building swig-core-utils.c
INCLUDE_DIRECTORIES (${CMAKE_CURRENT_BINARY_DIR}) # for gncla-dir.h

# Command to generate the swig-engine.c wrapper file
SET (SWIG_CORE_UTILS_C ${CMAKE_CURRENT_BINARY_DIR}/swig-core-utils.c)
GNC_ADD_SWIG_COMMAND (${SWIG_CORE_UTILS_C} ${CMAKE_CURRENT_SOURCE_DIR}/core-utils.i)

SET (libgnc_core_utils_SOURCES
  binreloc.c
  gnc-filepath-utils.c
  gnc-gconf-utils.c
  gnc-gdate-utils.c
  gnc-gkeyfile-utils.c
  gnc-glib-utils.c
  gnc-main.c
  gnc-path.c
  gnc-uri-utils.c
  ${SWIG_CORE_UTILS_C}
)

# Add dependency on config.h
SET_SOURCE_FILES_PROPERTIES (${libgnc_core_utils_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

# Workaround to create a very simple gncla-dir.h file
FILE (WRITE ${CMAKE_CURRENT_BINARY_DIR}/gncla-dir.h.tmp "
#define PREFIX \"${CMAKE_INSTALL_PREFIX}\"
#define DATADIR \"${CMAKE_INSTALL_PREFIX}/share\"
#define SYSCONFDIR \"${CMAKE_INSTALL_PREFIX}/etc\"
#define LIBDIR \"${CMAKE_INSTALL_PREFIX}/lib\"
#define LOCALE_DATADIRNAME \"share\"
")
# Let cmake copy the created file only on changes.
CONFIGURE_FILE (${CMAKE_CURRENT_BINARY_DIR}/gncla-dir.h.tmp ${CMAKE_CURRENT_BINARY_DIR}/gncla-dir.h COPYONLY)

SET (libgnc_core_utils_HEADERS
  binreloc.h
  gnc-filepath-utils.h
  gnc-gconf-utils.h
  gnc-gdate-utils.h
  gnc-gkeyfile-utils.h
  gnc-glib-utils.h
  gnc-main.h
  gnc-path.h
  gnc-uri-utils.h
)

ADD_LIBRARY	(core-utils
  ${libgnc_core_utils_SOURCES}
  ${libgnc_core_utils_HEADERS}
  )
