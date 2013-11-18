# CMakeLists.txt for src/engine

ADD_DEFINITIONS (-DG_LOG_DOMAIN=\"gnc.app-utils\")

INCLUDE_DIRECTORIES (${GLIB2_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${GCONF2_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${LIBINTL_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${REGEX_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${GUILE_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${HTMLHELP_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${CMAKE_BINARY_DIR}/src ) # for config.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/lib/libc) # for pow.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src ) # for gnc-ui.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/libqof/qof) # for qof.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/gnc-module) # for gnc-glib-utils.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/core-utils) # for gnc-glib-utils.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/app-utils/calculation) # for finproto.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/engine) # for gnc-engine.h
INCLUDE_DIRECTORIES (${CMAKE_BINARY_DIR}/src/engine) # for swig-runtime.h
INCLUDE_DIRECTORIES (${CMAKE_CURRENT_SOURCE_DIR}) # for <option-util.h>

SET (app_utils_HEADERS
  QuickFill.h
  file-utils.h
  gfec.h
  gnc-basic-gobject.h
  gnc-account-merge.h
  gnc-accounting-period.h
  gnc-addr-quickfill.h
  gnc-component-manager.h
  gnc-entry-quickfill.h
  gnc-euro.h
  gnc-exp-parser.h
  gnc-gettext-util.h
  gnc-help-utils.h
  gnc-helpers.h
  gnc-sx-instance-model.h
  gnc-ui-util.h
  gnc-ui-balances.h
  guile-util.h
  option-util.h
  calculation/finvar.h
  calculation/finproto.h
  calculation/fin_spl_protos.h
  calculation/fin_static_proto.h
)

# Command to generate the swig-app-utils.c wrapper file
SET (SWIG_APP_UTILS_C ${CMAKE_CURRENT_BINARY_DIR}/swig-app-utils.c)
GNC_ADD_SWIG_COMMAND (${SWIG_APP_UTILS_C} ${CMAKE_CURRENT_SOURCE_DIR}/app-utils.i)

SET (app_utils_SOURCES
  calculation/expression_parser.c
  calculation/fin.c
  QuickFill.c
  file-utils.c
  gfec.c
  gnc-account-merge.c
  gnc-accounting-period.c
  gnc-addr-quickfill.c
  gnc-component-manager.c
  gnc-entry-quickfill.c
  gnc-euro.c
  gnc-exp-parser.c
  gnc-gettext-util.c
  gnc-helpers.c
  gnc-sx-instance-model.c
  gnc-ui-util.c
  gnc-ui-balances.c
  gncmod-app-utils.c
  guile-util.c
  option-util.c
  ${SWIG_APP_UTILS_C}
)

IF (WIN32)
  SET (app_utils_SOURCES ${app_utils_SOURCES} gnc-help-utils.c)
ENDIF (WIN32)

ADD_LIBRARY	(app-utils
  ${app_utils_SOURCES}
  ${app_utils_HEADERS}
  )
