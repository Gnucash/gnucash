# CMakeLists.txt for src/libqof

# EXPERIMENTAL! This is just a trial of how far we can get for a cmake
# build system.

ADD_DEFINITIONS (-DG_LOG_DOMAIN=\"gnc.core-utils\")

INCLUDE_DIRECTORIES (${GLIB2_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${GCONF2_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${LIBINTL_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${REGEX_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${CMAKE_BINARY_DIR} ) # for config.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}) # for gnc-ui.h

# FIXME: I still need to look up SWIG wrapper generation in cmake...
#SWIG_ADD_MODULE (core_utils c swig-core-utils.i swig-core-utils.c)

SET (libgnc_core_utils_SOURCES
  gnc-main.c 
  gnc-gconf-utils.c 
  gnc-gdate-utils.c 
  gnc-gkeyfile-utils.c 
  gnc-glib-utils.c 
  ../gnc-module/gnc-module.c
)

SET (libgnc_core_utils_HEADERS
  gnc-main.h 
  gnc-gconf-utils.h 
  gnc-gdate-utils.h 
  gnc-gkeyfile-utils.h 
  gnc-glib-utils.h
  ../gnc-module/gnc-module.h
  ../gnc-module/gnc-module-api.h
)

ADD_LIBRARY	(core-utils
  ${libgnc_core_utils_SOURCES}
  ${libgnc_core_utils_HEADERS}
  )

