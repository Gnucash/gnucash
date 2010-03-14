# CMakeLists.txt for src/calculation

INCLUDE_DIRECTORIES (${GLIB2_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${LIBINTL_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${REGEX_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${GUILE_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${CMAKE_BINARY_DIR}/src ) # for config.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src ) # for gnc-ui.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/libqof/qof) # for qof.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/lib/libc) # for pow.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/gnc-module) # for gnc-module.h

SET (calculation_SOURCES
  expression_parser.c
  fin.c
  gncmod-calculation.c
)

# Add dependency on config.h
SET_SOURCE_FILES_PROPERTIES (${calculation_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

SET (calculation_HEADERS
  finvar.h
  finproto.h
  fin_spl_protos.h
  fin_static_proto.h
)

ADD_LIBRARY	(calculation
  ${calculation_SOURCES}
  ${calculation_HEADERS}
  )
