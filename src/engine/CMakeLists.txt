# CMakeLists.txt for src/libqof

# EXPERIMENTAL! This is just a trial of how far we can get for a cmake
# build system.

ADD_DEFINITIONS (-DG_LOG_DOMAIN=\"gnc.engine\")

INCLUDE_DIRECTORIES (${GLIB2_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${LIBINTL_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${REGEX_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${CMAKE_BINARY_DIR} ) # for config.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}) # for gnc-ui.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/libqof/qof) # for qof.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/gnc-module) # for gnc-glib-utils.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/core-utils) # for gnc-glib-utils.h
INCLUDE_DIRECTORIES (${CMAKE_CURRENT_SOURCE_DIR}) # for <Account.h>
INCLUDE_DIRECTORIES (${CMAKE_CURRENT_BINARY_DIR}) # for gncla-dir.h

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
  binreloc.h 
  cap-gains.h 
  cashobjects.h 
  engine-helpers.h 
  glib-helpers.h 
  gnc-associate-account.h 
  gnc-book.h 
  gnc-budget.h 
  gnc-commodity.h 
  gnc-engine.h 
  gnc-event.h 
  gnc-filepath-utils.h 
  gnc-hooks.h 
  gnc-path.h 
  gnc-pricedb.h 
  gnc-session.h 
  gnc-session-scm.h 
  gncObject.h 
  kvp-scm.h 
  policy.h
)

# Command to generate the swig-engine.c wrapper file
SET (SWIG_ENGINE_C ${CMAKE_CURRENT_BINARY_DIR}/swig-engine.c)
GNC_ADD_SWIG_COMMAND (${SWIG_ENGINE_C} ${CMAKE_CURRENT_SOURCE_DIR}/engine.i)

# Workaround to create a very simple gncla-dir.h file
FILE (WRITE ${CMAKE_CURRENT_BINARY_DIR}/gncla-dir.h "
#define PREFIX \"${CMAKE_INSTALL_PREFIX}\"
#define DATADIR \"${CMAKE_INSTALL_PREFIX}/share\"
#define SYSCONFDIR \"${CMAKE_INSTALL_PREFIX}/etc\"
#define LIBDIR \"${CMAKE_INSTALL_PREFIX}/lib\"
#define LOCALE_DATADIRNAME \"share\"
")

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
  binreloc.c 
  cap-gains.c 
  cashobjects.c 
  gnc-associate-account.c 
  gnc-budget.c 
#  gnc-commodity.c 
  gnc-engine.c 
  gnc-filepath-utils.c 
  gnc-hooks.c 
  gnc-lot.c 
  gnc-path.c 
  gnc-pricedb.c 
  gnc-session.c 
  gnc-session-scm.c 
  gncmod-engine.c 
  kvp-scm.c 
  engine-helpers.c 
  glib-helpers.c  
  policy.c
  ${SWIG_ENGINE_C}
)

ADD_LIBRARY	(engine
  ${libgncmod_engine_SOURCES}
  ${libgncmod_engine_HEADERS}
  )

# We depend on the swig-runtime-h target
ADD_DEPENDENCIES (engine swig-runtime-h)
