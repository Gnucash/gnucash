# CMakeLists.txt for the cutecash sub-project of gnucash

# This is very EXPERIMENTAL! We will see how far this project will
# get.

CMAKE_MINIMUM_REQUIRED (VERSION 2.6)
PROJECT (cutecash)

# Version number of cutecash
SET (CUTECASH_VERSION_MAJOR "0")
SET (CUTECASH_VERSION_MINOR "1")
SET (CUTECASH_VERSION_PATCH "0")

# Change this in development versions when changing anything that
# affects stored data structures. Reset to zero when bumping version.
SET (CUTECASH_VERSION_NANO "0")
SET (GNUCASH_RESAVE_VERSION "19920")

# Extra cmake macros
SET (CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/src/cmake_modules;${CMAKE_MODULE_PATH}")
INCLUDE (MacroAppendForeach)
INCLUDE (MacroAddSourceFileCompileFlags)
INCLUDE (GncAddSwigCommand)
INCLUDE (CheckIncludeFiles)

# ############################################################

OPTION (WITH_SQL "Build this project with SQL (libdbi) support" OFF)
OPTION (WITH_AQBANKING "Build this project with aqbanking (online banking) support" OFF)

# ############################################################

# Find pkg-config
FIND_PACKAGE (PkgConfig REQUIRED)

IF (NOT PKG_CONFIG_FOUND)
   MESSAGE (SEND_ERROR "pkg-config not found, but is required")
ENDIF (NOT PKG_CONFIG_FOUND)

# glib et al.
PKG_CHECK_MODULES (GLIB2 glib-2.0>=2.20)
PKG_CHECK_MODULES (GCONF2 gconf-2.0>=2.22)
PKG_CHECK_MODULES (GOBJECT gobject-2.0>=2.20)
PKG_CHECK_MODULES (GMODULE gmodule-2.0>=2.20)
PKG_CHECK_MODULES (GTHREAD gthread-2.0>=2.20)
PKG_CHECK_MODULES (GLIBMM glibmm-2.4>=2.24)
PKG_CHECK_MODULES (LIBXML2 libxml-2.0>=2.7.0)

IF (MSVC)
  MESSAGE (STATUS "Hint: To create the import libraries for the gnome DLLs (e.g. gconf-2.lib), use the dlltool as follows: pexports bin/libgconf-2-4.dll > lib/libgconf-2.def ; dlltool -d lib/libgconf-2.def -D bin/libgconf-2-4.dll -l lib/gconf-2.lib")

  # Disable the obnoxious min/max macros in MSVC - we want to use the
  # function versions of them.
  ADD_DEFINITIONS ( -DNOMINMAX )
ENDIF (MSVC)

## Libxml2
#FIND_PACKAGE (LibXml2 REQUIRED)
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
IF (WIN32)
  FIND_PATH (HTMLHELP_INCLUDE_PATH NAMES htmlhelp.h
             PATHS /usr/include)
ENDIF (WIN32)

# ############################################################

# SWIG
FIND_PACKAGE (SWIG REQUIRED)
INCLUDE (${SWIG_USE_FILE})

# guile library and include dir
PKG_CHECK_MODULES (GUILE guile-1.8>=1.8.0)
ADD_DEFINITIONS (-DHAVE_GUILE18)
# We also need to look up the executable
FIND_PROGRAM (GUILE_EXECUTABLE guile)

IF (NOT GUILE_FOUND)
  MESSAGE (SEND_ERROR "Guile was not found, but is required. Please set PKG_CONFIG_PATH so that guile-1.8.pc is found.")
ENDIF (NOT GUILE_FOUND)
IF (NOT GUILE_EXECUTABLE)
  MESSAGe (SEND_ERROR "The guile executable was not found, but is required. Please set GUILE_EXECUTABLE.")
ENDIF (NOT GUILE_EXECUTABLE)

#BOOST
SET (Boost_USE_STATIC_LIBS OFF)
SET (Boost_USE_MULTITHREADED ON)
SET (Boost_USE_STATIC_RUNTIME OFF)
SET (Boost_FIND_QUIETLY ON)
FIND_PACKAGE (Boost 1.50.0 REQUIRED COMPONENTS chrono date_time filesystem log program_options regex signals system) # test)
# boost-test currently can't be found by FindBoost.cmake because the
# shared library might be named libboost_unit_test_framework.so

IF (Boost_FOUND)
    include_directories(${Boost_INCLUDE_DIRS})
#    add_executable(progname file1.cxx file2.cxx)
#    target_link_libraries(progname ${Boost_LIBRARIES})
ELSE (Boost_FOUND)
    MESSAGE (SEND_ERROR "Boost 1.50.0 or later is not installed, and is required. Please install it and ensure that the following libraries are built: chrono, date_time, filesystem, log, program_options, regex, signals, system, and test.")
ENDIF (Boost_FOUND)

# Also, set the C++ version to c++11
SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

# ############################################################

# libdbi
FIND_PATH (LIBDBI_INCLUDE_PATH dbi/dbi.h)
FIND_LIBRARY (LIBDBI_LIBRARY dbi)
IF (WITH_SQL)
  IF (NOT LIBDBI_INCLUDE_PATH)
    MESSAGE (SEND_ERROR "Include file <dbi/dbi.h> was not found - did you install libdbi0-dev or libdbi-dev?")
  ENDIF (NOT LIBDBI_INCLUDE_PATH)
  IF (NOT LIBDBI_LIBRARY)
    MESSAGE (SEND_ERROR "Library libdbi was not found")
  ENDIF (NOT LIBDBI_LIBRARY)
ENDIF (WITH_SQL)

# ############################################################

# Qt
SET (QT_MIN_VERSION "4.5.0")
FIND_PACKAGE (Qt4 REQUIRED) # find and setup Qt4 for this project

# ############################################################
IF (WITH_AQBANKING)
  PKG_CHECK_MODULES (GWENHYWFAR REQUIRED gwenhywfar)
  PKG_CHECK_MODULES (AQBANKING REQUIRED aqbanking)
ENDIF (WITH_AQBANKING)

# ############################################################

# Compiler flags
IF (UNIX)
  SET( CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Werror -Wdeclaration-after-statement -Wno-pointer-sign -D_FORTIFY_SOURCE=2 -Wall -Wunused -Wmissing-prototypes -Wmissing-declarations  -Wno-unused")
  SET( CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wno-error=deprecated-declarations")
ENDIF (UNIX)
IF (MINGW)
  SET( CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Werror -Wdeclaration-after-statement -Wall -Wunused -Wmissing-prototypes -Wmissing-declarations  -Wno-unused")
ENDIF (MINGW)

# ############################################################

# The subdirectories
ADD_SUBDIRECTORY (src)


# ############################################################
# Package creation rules

SET (CPACK_PACKAGE_NAME "Cutecash")
SET (CPACK_PACKAGE_DESCRIPTION_SUMMARY "Cutecash Free Finance Software")
SET (CPACK_PACKAGE_VERSION_MAJOR ${CUTECASH_VERSION_MAJOR})
SET (CPACK_PACKAGE_VERSION_MINOR ${CUTECASH_VERSION_MINOR})
SET (CPACK_PACKAGE_VERSION_PATCH ${CUTECASH_VERSION_PATCH})
SET (CPACK_PACKAGE_VENDOR "Christian Stimming")
SET (CPACK_SOURCE_IGNORE_FILES "/\\\\.svn/;/\\\\.git/;.*~;build.*;html;Debug;Release")
SET (CPACK_STRIP_FILES "bin/cutecash")
#SET (CPACK_PACKAGE_EXECUTABLES "bin/carclient")

IF(UNIX)
  SET(CPACK_GENERATOR "TGZ")
ENDIF(UNIX)

INCLUDE (CPack)
