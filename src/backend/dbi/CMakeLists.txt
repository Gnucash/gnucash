# CMakeLists.txt for src/backend/dbi

ADD_DEFINITIONS (-DG_LOG_DOMAIN=\"gnc.backend.dbi\")

INCLUDE_DIRECTORIES (${GCONF2_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${GLIB2_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${LIBINTL_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${REGEX_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${LIBDBI_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${CMAKE_BINARY_DIR}/src ) # for config.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src ) # for gnc-ui.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/lib/libc) # for gnc-ui.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/libqof/qof)
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/core-utils)
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/engine)
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/backend/sql)

SET (libgnc_backend_dbi_SOURCES
  gnc-backend-dbi.c
)
SET (libgnc_backend_dbi_HEADERS
  gnc-backend-dbi.h
)

# Add dependency on config.h
SET_SOURCE_FILES_PROPERTIES (${libgnc_backend_dbi_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

ADD_LIBRARY	(gnc-backend-dbi
  ${libgnc_backend_dbi_SOURCES}
  ${libgnc_backend_dbi_HEADERS}
  )
