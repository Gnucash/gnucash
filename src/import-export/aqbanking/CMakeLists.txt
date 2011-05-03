# CMakeLists.txt for src/import-export/aqbanking

# ############################################################

ADD_DEFINITIONS (-DG_LOG_DOMAIN=\"gnc.import.aqbanking\")

INCLUDE_DIRECTORIES (${GLIB2_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${GCONF2_INCLUDE_DIRS})
#INCLUDE_DIRECTORIES (${LIBINTL_INCLUDE_PATH})
#INCLUDE_DIRECTORIES (${REGEX_INCLUDE_PATH})
#INCLUDE_DIRECTORIES (${GUILE_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${GWENHYWFAR_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${AQBANKING_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${CMAKE_BINARY_DIR}/src ) # for config.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/lib/libc) # for pow.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src ) # for gnc-ui.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/libqof/qof) # for qof.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/gnc-module) # for gnc-module.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/core-utils) # for gnc-glib-utils.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/engine)

SET (libgnc_aqbanking_SOURCES
  gnc-ab-kvp.c
#  gnc-ab-utils.c
#  gnc-gwen-gui.c
)

# Add dependency on config.h
SET_SOURCE_FILES_PROPERTIES (${libgnc_aqbanking_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

SET (libgnc_aqbanking_HEADERS
  gnc-ab-kvp.h
#  gnc-ab-utils.h
#  gnc-gwen-gui.h
)

ADD_LIBRARY (gnc-aqbanking
  ${libgnc_aqbanking_SOURCES}
  ${libgnc_aqbanking_HEADERS}
  )
