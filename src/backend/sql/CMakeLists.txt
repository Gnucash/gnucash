# CMakeLists.txt for src/backend/dbi

ADD_DEFINITIONS (-DG_LOG_DOMAIN=\"gnc.backend.sql\")

INCLUDE_DIRECTORIES (${GCONF2_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${GLIB2_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${LIBINTL_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${LIBGUILE_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${REGEX_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${CMAKE_BINARY_DIR}/src ) # for config.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src ) # for gnc-ui.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/lib/libc) # for gnc-ui.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/libqof/qof)
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/core-utils)
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/gnc-module)
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/engine)

SET (libgnc_backend_sql_SOURCES
  gnc-backend-sql.c
  gnc-account-sql.c
  gnc-book-sql.c
  gnc-budget-sql.c
  gnc-commodity-sql.c
  gnc-lots-sql.c
  gnc-price-sql.c
  gnc-recurrence-sql.c
  gnc-schedxaction-sql.c
  gnc-slots-sql.c
  gnc-transaction-sql.c
  escape.c
)
SET (libgnc_backend_sql_HEADERS
  gnc-account-sql.h
  gnc-backend-sql.h
  gnc-book-sql.h
  gnc-budget-sql.h
  gnc-commodity-sql.h
  gnc-lots-sql.h
  gnc-price-sql.h
  gnc-recurrence-sql.h
  gnc-schedxaction-sql.h
  gnc-slots-sql.h
  gnc-transaction-sql.h
  escape.h
)

# Add dependency on config.h
SET_SOURCE_FILES_PROPERTIES (${libgnc_backend_sql_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

ADD_LIBRARY	(gnc-backend-sql
  ${libgnc_backend_sql_SOURCES}
  ${libgnc_backend_sql_HEADERS}
  )
