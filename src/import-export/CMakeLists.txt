# CMakeLists.txt for src/import-export

# ############################################################

IF (WITH_AQBANKING)
  ADD_SUBDIRECTORY (aqb)
ENDIF (WITH_AQBANKING)

ADD_DEFINITIONS (-DG_LOG_DOMAIN=\"gnc.import\")

INCLUDE_DIRECTORIES (${GLIB2_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${GCONF2_INCLUDE_DIRS})
#INCLUDE_DIRECTORIES (${LIBINTL_INCLUDE_PATH})
#INCLUDE_DIRECTORIES (${REGEX_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${CMAKE_BINARY_DIR}/src ) # for config.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/lib/libc) # for pow.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src ) # for gnc-ui.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/libqof/qof) # for qof.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/gnc-module) # for gnc-module.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/core-utils) # for gnc-glib-utils.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/engine)
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/app-utils)

SET (libgnc_import_SOURCES
  import-utilities.c
  import-settings.c
)

# Add dependency on config.h
SET_SOURCE_FILES_PROPERTIES (${libgnc_import_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

SET (libgnc_import_HEADERS
  import-utilities.h
  import-settings.h
)

ADD_LIBRARY (gnc-import
  ${libgnc_import_SOURCES}
  ${libgnc_import_HEADERS}
  )

