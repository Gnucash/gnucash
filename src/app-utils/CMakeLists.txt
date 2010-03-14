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
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/calculation) # for finproto.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/engine) # for gnc-engine.h
INCLUDE_DIRECTORIES (${CMAKE_BINARY_DIR}/src/engine) # for swig-runtime.h
INCLUDE_DIRECTORIES (${CMAKE_CURRENT_SOURCE_DIR}) # for <option-util.h>

SET (app_utils_HEADERS
  file-utils.h
  gfec.h
  gnc-basic-gobject.h
  gnc-account-merge.h
  gnc-accounting-period.h
  gnc-component-manager.h
  gnc-druid.h
  gnc-druid-cb.h
  gnc-druid-provider.h
  gnc-druid-provider-desc.h
  gnc-druid-provider-desc-edge.h
  gnc-druid-provider-desc-file.h
  gnc-druid-provider-desc-multifile.h
  gnc-druid-provider-file-cb.h
  gnc-exp-parser.h
  gnc-gettext-util.h
  gnc-help-utils.h
  gnc-helpers.h
  gnc-sx-instance-model.h
  gnc-ui-common.h
  guile-util.h
  option-util.h
)

# Command to generate the swig-app-utils.c wrapper file
SET (SWIG_APP_UTILS_C ${CMAKE_CURRENT_BINARY_DIR}/swig-app-utils.c)
GNC_ADD_SWIG_COMMAND (${SWIG_APP_UTILS_C} ${CMAKE_CURRENT_SOURCE_DIR}/app-utils.i)

SET (app_utils_SOURCES
  file-utils.c
  gfec.c
  gnc-account-merge.c
  gnc-accounting-period.c
  gnc-component-manager.c
  gnc-druid.c
  gnc-druid-cb.c
  gnc-druid-provider.c
  gnc-druid-provider-desc.c
  gnc-druid-provider-desc-edge.c
  gnc-druid-provider-desc-file.c
  gnc-druid-provider-desc-multifile.c
  gnc-druid-provider-file-cb.c
  gnc-exp-parser.c
  gnc-gettext-util.c
  gnc-helpers.c
  gnc-sx-instance-model.c
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
