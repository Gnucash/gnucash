# CMakeLists.txt for the cutecash sub-project of gnucash

# This is very EXPERIMENTAL! We will see how far this project will
# get.

CMAKE_MINIMUM_REQUIRED (VERSION 2.6)
PROJECT (cutecash)

# Extra cmake macros
SET (CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/src/cmake_modules;${CMAKE_MODULE_PATH}")
INCLUDE (MacroAppendForeach)
INCLUDE (MacroAddSourceFileCompileFlags)
INCLUDE (GncAddSwigCommand)

# ############################################################

# Find pkg-config
FIND_PACKAGE (PkgConfig REQUIRED)

IF (NOT PKG_CONFIG_FOUND)
   MESSAGE (SEND_ERROR "pkg-config not found, but is required")
ENDIF (NOT PKG_CONFIG_FOUND)

# glib et al.
PKG_CHECK_MODULES (GLIB2 glib-2.0>=2.12.0)
PKG_CHECK_MODULES (GCONF2 gconf-2.0>=2.0)
PKG_CHECK_MODULES (GOBJECT gobject-2.0>=2.12.0)
PKG_CHECK_MODULES (GMODULE gmodule-2.0>=2.12.0)
PKG_CHECK_MODULES (GTHREAD gthread-2.0>=2.12.0)

IF (MSVC)
  MESSAGE (STATUS "Hint: To create the import libraries for the gnome DLLs (e.g. gconf-2.lib), use the dlltool as follows: pexports bin/libgconf-2-4.dll > lib/libgconf-2.def ; dlltool -d lib/libgconf-2.def -D bin/libgconf-2-4.dll -l lib/gconf-2.lib")
ENDIF (MSVC)

# Libxml2
FIND_PACKAGE (LibXml2 REQUIRED)
FIND_PACKAGE (ZLIB REQUIRED)

IF (NOT GLIB2_FOUND)
   MESSAGE (SEND_ERROR "Glib2 not found, but is required")
ENDIF (NOT GLIB2_FOUND)

FIND_PATH (LIBINTL_INCLUDE_PATH NAMES libintl.h
		  PATHS /usr/include /opt/gnome/include)
FIND_LIBRARY (LIBINTL_LIBRARY NAMES intl)
FIND_PATH (REGEX_INCLUDE_PATH NAMES regex.h
		  PATHS /usr/include /opt/gnome/include)
FIND_LIBRARY (REGEX_LIBRARY NAMES regex)

# ############################################################

# SWIG
FIND_PACKAGE (SWIG REQUIRED)
INCLUDE (${SWIG_USE_FILE})

# guile
FIND_PATH (LIBGUILE_INCLUDE_PATH NAMES libguile.h
		  PATHS /usr/include)
FIND_LIBRARY (LIBGUILE_LIBRARIES NAMES guile)
FIND_PROGRAM (GUILE_EXECUTABLE guile)

# ############################################################

# Qt
SET (QT_MIN_VERSION "4.5.0")
FIND_PACKAGE (Qt4 REQUIRED) # find and setup Qt4 for this project

# ############################################################

# Compiler flags
IF (UNIX)
  SET( CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Werror -Wdeclaration-after-statement -Wno-pointer-sign -D_FORTIFY_SOURCE=2 -Wall -Wunused -Wmissing-prototypes -Wmissing-declarations  -Wno-unused")
ENDIF (UNIX)
IF (MINGW)
  SET( CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Werror -Wdeclaration-after-statement -Wall -Wunused -Wmissing-prototypes -Wmissing-declarations  -Wno-unused")
ENDIF (MINGW)

# ############################################################

# The subdirectories
ADD_SUBDIRECTORY (src)
