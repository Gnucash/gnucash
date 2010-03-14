# CMakeLists.txt for src/engine

ADD_DEFINITIONS (-DG_LOG_DOMAIN=\"gnc.engine\")

INCLUDE_DIRECTORIES (${GLIB2_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${LIBINTL_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${REGEX_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${GUILE_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${CMAKE_BINARY_DIR}/src ) # for config.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/lib/libc) # for pow.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src ) # for gnc-ui.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/libqof/qof) # for qof.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/gnc-module) # for gnc-module.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/core-utils) # for gnc-glib-utils.h
INCLUDE_DIRECTORIES (${CMAKE_CURRENT_SOURCE_DIR}) # for <Account.h>
INCLUDE_DIRECTORIES (${CMAKE_CURRENT_BINARY_DIR}) # for iso-4217-currencies.c

SET (libgncmod_engine_HEADERS
  Account.h
  FreqSpec.h
  Recurrence.h
  GNCId.h
  Period.h
  SchedXaction.h
  SX-book.h
  SX-ttinfo.h
  Query.h
  QueryNew.h
  QueryObject.h
  QueryCore.h
  Scrub.h
  Scrub2.h
  Scrub3.h
  Split.h
  TransLog.h
  Transaction.h
  cap-gains.h
  cashobjects.h
  engine-helpers.h
  glib-helpers.h
  gnc-associate-account.h
  gnc-budget.h
  gnc-commodity.h
  gnc-engine.h
  gnc-event.h
  gnc-hooks.h
  gnc-pricedb.h
  gnc-session-scm.h
  gnc-session.h
  gncObject.h
  kvp-scm.h
  policy.h
)

# Command to generate the swig-engine.c wrapper file
SET (SWIG_ENGINE_C ${CMAKE_CURRENT_BINARY_DIR}/swig-engine.c)
GNC_ADD_SWIG_COMMAND (${SWIG_ENGINE_C} ${CMAKE_CURRENT_SOURCE_DIR}/engine.i)

# Command to generate the iso-4217-currencies.c file
SET (ISO_4217_C ${CMAKE_CURRENT_BINARY_DIR}/iso-4217-currencies.c)
ADD_CUSTOM_COMMAND (
  OUTPUT ${ISO_4217_C}
  DEPENDS iso-4217-currencies.scm iso-currencies-to-c
  COMMAND
    GUILE_LOAD_PATH=@GNC_SRFI_LOAD_PATH@:${GUILE_LOAD_PATH}
	srcdir=${CMAKE_CURRENT_SOURCE_DIR}
	${GUILE_EXECUTABLE} -s ${CMAKE_CURRENT_SOURCE_DIR}/iso-currencies-to-c
)
# Add dependency on iso-4217-currencies.c
SET_SOURCE_FILES_PROPERTIES (gnc-commodity.c PROPERTIES OBJECT_DEPENDS ${ISO_4217_C})

# Command to generate the swig-runtime.h header
SET (SWIG_RUNTIME_H ${CMAKE_CURRENT_BINARY_DIR}/swig-runtime.h)
ADD_CUSTOM_COMMAND (
  OUTPUT ${SWIG_RUNTIME_H}
  DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/CMakeLists.txt
  COMMAND ${SWIG_EXECUTABLE} -guile -external-runtime ${SWIG_RUNTIME_H}
)
# Add dependency on swig-runtime.h
SET_SOURCE_FILES_PROPERTIES (gnc-hooks.c engine-helpers.c kvp-scm.c glib-helpers.c PROPERTIES OBJECT_DEPENDS ${SWIG_RUNTIME_H})

SET (libgncmod_engine_SOURCES
  Account.c
  Recurrence.c
  Period.c
  Query.c
  SchedXaction.c
  SX-book.c
  SX-ttinfo.c
  Scrub.c
  Scrub2.c
  Scrub3.c
  Split.c
  TransLog.c
  Transaction.c
  cap-gains.c
  cashobjects.c
  gnc-associate-account.c
  gnc-budget.c
  gnc-commodity.c
  gnc-engine.c
  gnc-hooks.c
  gnc-lot.c
  gnc-pricedb.c
  gnc-session-scm.c
  gnc-session.c
  gncmod-engine.c
  kvp-scm.c
  engine-helpers.c
  glib-helpers.c
  policy.c
  ${SWIG_ENGINE_C}
)

# Add dependency on config.h
#SET_SOURCE_FILES_PROPERTIES (${libgncmod_engine_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

ADD_LIBRARY	(engine
  ${libgncmod_engine_SOURCES}
  ${libgncmod_engine_HEADERS}
  )
