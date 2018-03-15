# CMakeLists.txt for GnuCash

IF (WIN32 OR APPLE)
  CMAKE_MINIMUM_REQUIRED (VERSION 3.3.2)
ELSE()
  CMAKE_MINIMUM_REQUIRED (VERSION 3.0)
ENDIF()

PROJECT (gnucash)

ENABLE_TESTING()

# Version number of gnucash
SET (GNUCASH_MAJOR_VERSION 2)
SET (GNUCASH_MINOR_VERSION 7)
SET (GNUCASH_MICRO_VERSION 6)
SET (GNUCASH_NANO_VERSION 0)
SET (VERSION "${GNUCASH_MAJOR_VERSION}.${GNUCASH_MINOR_VERSION}.${GNUCASH_MICRO_VERSION}")
SET (GNUCASH_LATEST_STABLE_SERIES 2.6)

SET (PACKAGE gnucash)
SET (PACKAGE_NAME GnuCash)
SET (PACKAGE_VERSION 2.7.6)
SET (PACKAGE_BUGREPORT gnucash-devel@gnucash.org)
SET (PACKAGE_TARNAME ${PACKAGE})
SET (PACKAGE_STRING "${PACKAGE_NAME} ${PACKAGE_VERSION}")
SET (PACKAGE_URL "")

# Change this in development versions when changing anything that
# affects stored data structures. Reset to zero when bumping version.

SET (GNUCASH_RESAVE_VERSION "19920")

SET(GETTEXT_PACKAGE "gnucash")

# Clear cache variables that will be filled later during the cmake run
unset(dist_generated CACHE)
unset(dist_generated_depends CACHE)

# Extra cmake macros
SET (CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/common/cmake_modules;${CMAKE_MODULE_PATH}")
# CMake does a non-recursive build that puts the final build product directories in the build root. Some code needs to know this.

INCLUDE (MacroAppendForeach)
INCLUDE (MacroAddSourceFileCompileFlags)
INCLUDE (GncAddSwigCommand)
INCLUDE (CheckIncludeFiles)
INCLUDE (GncAddSchemeTargets)
INCLUDE (GncAddGSchemaTargets)
INCLUDE (GncAddTest)
INCLUDE (MakeDistFiles)
INCLUDE (GNUInstallDirs)

# ############################################################
# These options are settable from the CMake command line. For example, to disable
# SQL, put -D WITH_SQL=OFF on the command line.

OPTION (WITH_SQL "Build this project with SQL (libdbi) support" ON)
OPTION (WITH_AQBANKING "Build this project with aqbanking (online banking) support" ON)
OPTION (WITH_GNUCASH "Build all of GnuCash, not just the library" ON)
OPTION (WITH_OFX "compile with ofx support (needs LibOFX)" ON)
OPTION (WITH_PYTHON "enable python plugin and bindings" OFF)
OPTION (ENABLE_BINRELOC "compile with binary relocation support" ON)
OPTION (ENABLE_REGISTER2 "compile with register2 enabled" OFF)
OPTION (DISABLE_NLS "do not use Native Language Support" OFF)
OPTION (DISABLE_DEPRECATED_GLIB "don't use deprecated glib functions" OFF)
OPTION (DISABLE_DEPRECATED_GTK "don't use deprecated gtk, gdk or gdk-pixbuf functions" OFF)
OPTION (DISABLE_DEPRECATED_GNOME "don't use deprecated gnome functions" OFF)
OPTION (ALLOW_OLD_GETTEXT "allow to configure build with a gettext version older than 0.19.6. Some files will not be translated!" OFF)
# ############################################################

# These are also settable from the command line in a similar way.

SET(GNUCASH_BUILD_ID "" CACHE STRING "Overrides the GnuCash build identification (Build ID) which defaults to a description of the vcs commit from which gnucash is built. Distributions may want to insert a package management based version number instead")
# GENERATE_SWIG_WRAPPERS - Controls whether to generate the swig wrappers for guile and python. If not set the wrappers will only be generated when building from a git worktree, commented out here, but will be evaluated later on in this file

# Check that all of the absolute install paths are inside
# ${CMAKE_INSTALL_PREFIX}. If they're not, disable binreloc as it
# won't be able to find paths that aren't relative to the location of
# the executable.
foreach(install_dir ${CMAKE_INSTALL_FULL_BINDIR}
    ${CMAKE_INSTALL_FULL_SYSCONFDIR} ${CMAKE_INSTALL_FULL_DATAROOTDIR}
    ${CMAKE_INSTALL_FULL_DATADIR} ${CMAKE_INSTALL_FULL_LIBDIR})
  string(FIND ${install_dir} ${CMAKE_INSTALL_PREFIX} in_prefix)
  if(in_prefix EQUAL -1)
    set(ENABLE_BINRELOC OFF)
    message(WARNING "${install_dir} is set outside of the intallation prefix ${CMAKE_INSTALL_PREFIX}. That will break relocation so ENABLE_BINRELOC is set to off. With relocation disabled GnuCash will run only in its configured install location. You must set GNC_UNINSTALLED=1 and GNC_BUILDDIR=/path/to/builddir to run from the build directory. GnuCash will not run from a DESTDIR.")
    break()
  endif()
endforeach()

# GnuCash installs two files in ${CMAKE_INSTALL_SYSCONFDIR}
SET(BINDIR ${CMAKE_INSTALL_BINDIR} CACHE STRING "user executables")
SET(SYSCONFDIR ${CMAKE_INSTALL_SYSCONFDIR} CACHE STRING "read-only single-machine data")
SET(DATAROOTDIR ${CMAKE_INSTALL_DATAROOTDIR} CACHE STRING "read-only arch.-independent data root")
SET(DATADIR ${CMAKE_INSTALL_DATADIR} CACHE STRING "read-only architecture-independent data")
SET(LIBDIR ${CMAKE_INSTALL_LIBDIR} CACHE STRING "object code libraries")
SET(LOCALEDIR ${DATAROOTDIR}/locale CACHE STRING "locale-dependent data")
SET(GNC_HELPDIR ${DATADIR} CACHE STRING "where to store help files")
SET(DATADIRNAME share)
SET(GNC_SYSTEM_XDG_DATA_DIRS /usr/local/share /usr/share)
SET(GNC_DBD_DIR ${CMAKE_PREFIX_LIBDIR}/dbd CACHE PATH "specify location of libdbi drivers")
SET(PKGLIBDIR ${CMAKE_INSTALL_LIBDIR}/gnucash)
SET(TEST_MYSQL_URL "" CACHE STRING "MySQL database URL for testing")
SET(TEST_PGSQL_URL "" CACHE STRING "PgSQL database URL for testing")

SET(DATADIR_BUILD    ${CMAKE_BINARY_DIR}/${DATADIRNAME})
STRING(REPLACE ${CMAKE_INSTALL_PREFIX} "" LIBDIR_BUILD ${LIBDIR})
SET(LIBDIR_BUILD     ${CMAKE_BINARY_DIR}/${LIBDIR_BUILD})
SET(SYSCONFDIR_BUILD ${CMAKE_BINARY_DIR}/etc)
SET(LIBEXECDIR_BUILD ${CMAKE_BINARY_DIR}/libexec)
SET(BINDIR_BUILD     ${CMAKE_BINARY_DIR}/bin)

SET(SHELL_FROM_ENV $ENV{SHELL})
SET(SHELL /bin/bash)
#We need to distinguish between MinGW.org and MinGW-w64:

IF (MINGW)
  STRING(FIND ${CMAKE_C_COMPILER} "msys2" IS_MSYS2)
  IF(${IS_MSYS2} GREATER_EQUAL 0)
    SET(MINGW64 ON)
  ENDIF()
ENDIF(MINGW)
IF (MINGW AND NOT MINGW64) # /bin/bash will work fine on MinGW
  SET(SHELL ${CMAKE_PREFIX_PATH}/mingw/msys/1.0/bin/bash.exe)
ENDIF()
IF (SHELL_FROM_ENV) # Replacing this with IF ($ENV{SHELL}) doesn't work.
  SET(SHELL ${SHELL_FROM_ENV})
ENDIF()

# Determine whether we are building from a VCS or from a tarball
EXECUTE_PROCESS(
  COMMAND ${SHELL} ${CMAKE_SOURCE_DIR}/util/gnc-vcs-info -t ${CMAKE_SOURCE_DIR}
  RESULT_VARIABLE GNC_VCS_INFO_RESULT
  OUTPUT_VARIABLE GNC_VCS_INFO_OUTPUT
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

SET(BUILDING_FROM_VCS "NO")
IF (GNC_VCS_INFO_RESULT EQUAL 0)
  SET(BUILDING_FROM_VCS "YES")
  # IF building from VCS, we need git
  find_package(Git)
  IF (NOT GIT_FOUND)
    MESSAGE(SEND_ERROR "Looks like we're building from version control, but can't find git executable. Please set GIT_EXECUTABLE.")
  ENDIF()
ENDIF()

# Determine whether to generate the swig wrappers.
# By default they will only be generated when building from a git worktree
set(GENERATE_SWIG_WRAPPERS ${BUILDING_FROM_VCS} CACHE BOOL "Controls whether to generate the swig wrappers for guile and python. If not set the wrappers will only be generated when building from a git worktree")

IF (WIN32)
  # Help Windows find the various dependencies. We assume here that the standard advice for building
  # GnuCash has been followed and that these dependencies live underneath something like C:/GCDEV, which
  # should be provided as CMAKE_PREFIX_PATH on the CMake command line:
  #      cmake -D CMAKE_PREFIX_PATH=c/gcdev -G "MSYS Makefiles" path/to/gnucash/sources
  #
  SET(CMAKE_C_IMPLICIT_LINK_LIBRARIES "")
  SET(CMAKE_FRAMEWORK_PATH_TMP ${CMAKE_PREFIX_PATH})
  SET(DEV_SUBDIRS aqbanking gnome guile gwenhywfar libgsf libofx libsoup libxslt webkit)
  FOREACH(subdir ${DEV_SUBDIRS})
    LIST(APPEND CMAKE_FRAMEWORK_PATH_TMP ${CMAKE_PREFIX_PATH}/${subdir})
  ENDFOREACH()
  # Setting CMAKE_FRAMEWORK_PATH, which is intended for OS X, is a terrible hack here. But I don't
  # want to mess with CMAKE_PREFIX_PATH.
  SET(CMAKE_FRAMEWORK_PATH "${CMAKE_FRAMEWORK_PATH_TMP}")
  #SET(PKG_CONFIG_EXECUTABLE ${CMAKE_PREFIX_PATH}/gnome/bin/pkg-config.exe)
  SET(SWIG_EXECUTABLE swig)
  #SET(SWIG_EXECUTABLE ${CMAKE_PREFIX_PATH}/swig/swig.exe)
  FIND_PATH(REGEX_INCLUDE_PATH regex.h)
  FIND_PATH(REGEX_LIB_PATH libregex.dll.a)
  SET(REGEX_LDFLAGS "-L${REGEX_LIB_PATH} -lregex")
  #SET(LIBXSLT_INCLUDE_DIR ${CMAKE_PREFIX_PATH}/libxslt/include)
  #SET(LIBXSLT_XSLTPROC_EXECUTABLE ${CMAKE_PREFIX_PATH}/libxslt/bin/xsltproc)
#Prevent creating a console window on startup.
  SET(CMAKE_EXE_LINKER_FLAGS -mwindows)
ENDIF(WIN32)

FIND_PACKAGE(PkgConfig REQUIRED)

# The default FindPkgConfig.make code has a bug in how the setting of PKG_CONFIG_PATH is handled.
# The common/cmake_modules/GncFindPkgConfig.cmake file overrides a couple of macros in FindPkgConfig to fix.

INCLUDE (GncFindPkgConfig)

IF (NOT PKG_CONFIG_FOUND)
   MESSAGE (SEND_ERROR "pkg-config not found, but is required")
ENDIF (NOT PKG_CONFIG_FOUND)

# glib et al.
GNC_PKG_CHECK_MODULES (GLIB2 REQUIRED glib-2.0>=2.40)
GNC_PKG_CHECK_MODULES (GIO REQUIRED gio-2.0)
GNC_PKG_CHECK_MODULES (GOBJECT REQUIRED gobject-2.0>=2.20)
GNC_PKG_CHECK_MODULES (GMODULE REQUIRED gmodule-2.0>=2.20)
GNC_PKG_CHECK_MODULES (GTHREAD REQUIRED gthread-2.0>=2.20)

GNC_PKG_CHECK_MODULES (LIBXML2 REQUIRED libxml-2.0>=2.7.0)
GNC_PKG_CHECK_MODULES (LIBXSLT REQUIRED libxslt)
IF (WIN32 OR APPLE)
  GNC_PKG_CHECK_MODULES (WEBKIT1 REQUIRED webkitgtk-3.0)
  SET(WEBKIT1 1)
    SET(WEBKIT_CFLAGS ${WEBKIT2_CFLAGS})
    SET(WEBKIT_INCLUDE_DIRS ${WEBKIT1_INCLUDE_DIRS})
    SET(WEBKIT_LDFLAGS ${WEBKIT1_LDFLAGS})
    SET(WEBKIT_LIBRARIES ${WEBKIT1_LIBRARIES})
ELSE (WIN32 OR APPLE)
  GNC_PKG_CHECK_MODULES (WEBKIT2_4 webkit2gtk-4.0)
  IF (NOT WEBKIT2_4_FOUND)
    GNC_PKG_CHECK_MODULES (WEBKIT2_3 REQUIRED webkit2gtk-3.0)
    SET(WEBKIT2_3 1)
    SET(WEBKIT_CFLAGS ${WEBKIT2_3_CFLAGS})
    SET(WEBKIT_INCLUDE_DIRS ${WEBKIT2_3_INCLUDE_DIRS})
    SET(WEBKIT_LDFLAGS ${WEBKIT2_3_LDFLAGS})
    SET(WEBKIT_LIBRARIES ${WEBKIT2_3_LIBRARIES})
  ELSE (NOT WEBKIT2_4_FOUND)
    SET(WEBKIT2_4 1)
    SET(WEBKIT_CFLAGS ${WEBKIT2_4_CFLAGS})
    SET(WEBKIT_INCLUDE_DIRS ${WEBKIT2_4_INCLUDE_DIRS})
    SET(WEBKIT_LDFLAGS ${WEBKIT2_4_LDFLAGS})
    SET(WEBKIT_LIBRARIES ${WEBKIT2_4_LIBRARIES})
  ENDIF (NOT WEBKIT2_4_FOUND)
ENDIF (WIN32 OR APPLE)

IF (WITH_GNUCASH)
  GNC_PKG_CHECK_MODULES (GTK3 REQUIRED gtk+-3.0>=3.10.0)
ENDIF (WITH_GNUCASH)

GNC_PKG_CHECK_MODULES (ZLIB REQUIRED zlib)

IF (MSVC)
  MESSAGE (STATUS "Hint: To create the import libraries for the gnome DLLs (e.g. gconf-2.lib), use the dlltool as follows: pexports bin/libgconf-2-4.dll > lib/libgconf-2.def ; dlltool -d lib/libgconf-2.def -D bin/libgconf-2-4.dll -l lib/gconf-2.lib")

  # Disable the obnoxious min/max macros in MSVC - we want to use the
  # function versions of them.
  ADD_DEFINITIONS ( -DNOMINMAX )
ENDIF (MSVC)

FIND_PATH (LIBINTL_INCLUDE_PATH NAMES libintl.h
		  PATHS /usr/include /opt/gnome/include)
FIND_LIBRARY (LIBINTL_LIBRARY NAMES intl)

FIND_PATH (LTDL_INCLUDE_PATH NAMES ltdl.h PATHS /usr/include)

IF(NOT LTDL_INCLUDE_PATH-NOTFOUND)
  SET(HAVE_LTDL_H 1)
ENDIF()



FIND_PROGRAM(GLIB_COMPILE_SCHEMAS glib-compile-schemas HINTS ${CMAKE_PREFIX_PATH}/gnome/bin)
IF (NOT GLIB_COMPILE_SCHEMAS)
  MESSAGE(SEND_ERROR "Can't find glib-compile-schemas program. Please set GLIB_COMPILE_SCHEMAS.")
ENDIF(NOT GLIB_COMPILE_SCHEMAS)

if (ALLOW_OLD_GETTEXT)
    find_package (Gettext REQUIRED)
else (ALLOW_OLD_GETTEXT)
    find_package (Gettext 0.19.6)
    if (NOT GETTEXT_FOUND)
        message (FATAL_ERROR "Note the build can be configured with an older version of gettext by setting ALLOW_OLD_GETTEXT=ON but then some files will not be translated:
        - gnucash.desktop (requires at least gettext 0.19)
        - gnucash.appdata.xml (requires at least gettext 0.19.6)")
    endif (NOT GETTEXT_FOUND)
endif (ALLOW_OLD_GETTEXT)

if (${GETTEXT_VERSION_STRING} VERSION_LESS 0.19)
    message (WARNING "Got gettext version ${GETTEXT_VERSION_STRING}, however you need at least gettext version 0.19 in order to handle translation of the gnucash.desktop file. The build will be configured with an untranslated gnucash.desktop file.")
endif ()
if (${GETTEXT_VERSION_STRING} VERSION_LESS 0.19.6)
    message (WARNING "Got gettext version ${GETTEXT_VERSION_STRING}, however you need at least gettext version 0.19.6 in order to handle translation of the gnucash.appdata file. The build will be configured with an untranslated gnucash.appdata file.")
endif ()

FIND_PATH (REGEX_INCLUDE_PATH NAMES regex.h
		  PATHS /usr/include /opt/gnome/include)
FIND_LIBRARY (REGEX_LIBRARY NAMES regex)
IF (WIN32)
  message(STATUS "Looking for htmlhelp.h and htmlhelp.a")
  FIND_PATH (HTMLHELP_INCLUDE_PATH NAMES htmlhelp.h)
  FIND_LIBRARY (HTMLHELP_LIBRARY htmlhelp)
ENDIF (WIN32)

# ############################################################

# SWIG
IF(GENERATE_SWIG_WRAPPERS)
  FIND_PACKAGE (SWIG REQUIRED)
  INCLUDE (${SWIG_USE_FILE})
ENDIF()

# Find Guile and determine which version we are using.
# Look for guile versions in this order: 2.2 > 2.0

# guile library and include dir
GNC_PKG_CHECK_MODULES (GUILE22 guile-2.2 QUIET)
IF (GUILE22_FOUND) # found guile-2.2
  ADD_DEFINITIONS (-DHAVE_GUILE22)
  SET(HAVE_GUILE2 TRUE)
  SET(GUILE_EFFECTIVE_VERSION 2.2)
  SET(GUILE_INCLUDE_DIRS ${GUILE22_INCLUDE_DIRS})
  SET(GUILE_LDFLAGS ${GUILE22_LDFLAGS})

  FIND_PROGRAM (GUILD_EXECUTABLE NAMES guild2.2 guild)
  IF (NOT GUILD_EXECUTABLE)
    MESSAGE (SEND_ERROR "The guild executable was not found, but is required. Please set GUILD_EXECUTABLE.")
  ENDIF (NOT GUILD_EXECUTABLE)
  MESSAGE(STATUS "Using guile-2.2.x")
  FIND_PROGRAM (GUILE_EXECUTABLE NAMES guile2.2 guile)
ELSE(GUILE22_FOUND)
  GNC_PKG_CHECK_MODULES (GUILE2 guile-2.0>=2.0.9 QUIET)
  IF (GUILE2_FOUND) # found guile-2.0
    ADD_DEFINITIONS (-DHAVE_GUILE20)
    SET(HAVE_GUILE2 TRUE)
    SET(GUILE_EFFECTIVE_VERSION 2.0)
    SET(GUILE_INCLUDE_DIRS ${GUILE2_INCLUDE_DIRS})
    SET(GUILE_LDFLAGS ${GUILE2_LDFLAGS})

    FIND_PROGRAM (GUILD_EXECUTABLE NAMES guild2.0 guild)
    IF (NOT GUILD_EXECUTABLE)
      MESSAGE (SEND_ERROR "The guild executable was not found, but is required. Please set GUILD_EXECUTABLE.")
    ENDIF (NOT GUILD_EXECUTABLE)
    MESSAGE(STATUS "Using guile-2.0.x")
    FIND_PROGRAM (GUILE_EXECUTABLE NAMES guile2.0 guile)
  ELSE(GUILE2_FOUND)
    MESSAGE (FATAL_ERROR "Neither guile 2.2 nor guile 2.0 were found GnuCash can't run without one of them. Ensure that one is installed and can be found with pgk-config.")
  ENDIF(GUILE2_FOUND)
ENDIF(GUILE22_FOUND)

IF (NOT GUILE_EXECUTABLE)
  MESSAGE (SEND_ERROR "The guile executable was not found, but is required. Please set GUILE_EXECUTABLE.")
ENDIF (NOT GUILE_EXECUTABLE)

# Test that guile has SRFI-64. This is required for some unit tests.
execute_process (COMMAND ${GUILE_EXECUTABLE} -c "(use-modules (srfi srfi-64))"
  RESULT_VARIABLE GNC_SRFI64_RESULT
  ERROR_QUIET
)

if (GNC_SRFI64_RESULT EQUAL 0)
  message (STATUS "Using guile SRFI-64")
  set (HAVE_SRFI64 TRUE)
endif ()

# ############################################################
IF (WITH_AQBANKING)
  GNC_PKG_CHECK_MODULES (GWENHYWFAR REQUIRED gwenhywfar)
  GNC_PKG_CHECK_MODULES (AQBANKING REQUIRED aqbanking)
  IF(WITH_GNUCASH)
    GNC_PKG_CHECK_MODULES (KTOBLZCHECK ktoblzcheck)
    IF(KTOBLZCHECK_FOUND)
      SET(HAVE_KTOBLZCHECK_H 1)
    ENDIF(KTOBLZCHECK_FOUND)
    GNC_PKG_CHECK_MODULES (GWEN_GTK3 gwengui-gtk3)
    IF(GWEN_GTK3_FOUND)
      SET(HAVE_GWEN_GTK3 1 CACHE BOOL "True if gwen-gtk3.pc exists")
    ENDIF(GWEN_GTK3_FOUND)
  ENDIF(WITH_GNUCASH)
ENDIF (WITH_AQBANKING)

IF (WITH_OFX)
  GNC_PKG_CHECK_MODULES (LIBOFX REQUIRED libofx)
  INCLUDE(CheckCXXSourceRuns)
  IF (WIN32)
      SET(CMAKE_REQUIRED_LIBRARIES "-L ${CMAKE_PREFIX_PATH}/libofx/lib -lofx")
  ELSE (WIN32)
      SET(CMAKE_REQUIRED_LIBRARIES "-lofx")
  ENDIF (WIN32)
      CHECK_CXX_SOURCE_RUNS("
#include <time.h>
#include <stdlib.h>
#include <string>
extern time_t ofxdate_to_time_t(const std::string ofxdate);

int main(int argc, char** argv)
{
    const std::string timestr = \"20160319000000\";
    struct tm ts;
    ts.tm_year = 116;
    ts.tm_mon = 2;
    ts.tm_mday = 19;
#ifdef _WIN32
    putenv(\"TZ=PST-8PDT-7,M 4.1.0/0,M 10.6.0/0\");
#else
    setenv(\"TZ\", \"PST 08P DT 07 M 4.1.0, M 10.6.0\", 1);
#endif
    time_t t = ofxdate_to_time_t(timestr);
    if (t == mktime(&ts))
        exit(1);
    exit(0);
}
" HAVE_OFX_BUG_39)
  SET(HAVE_OFX_BUG_39 ${HAVE_OFX_BUG_39})
  SET(CMAKE_REQUIRED_LIBRARIES)
ENDIF(WITH_OFX)
# ############################################################

IF(APPLE)
  EXECUTE_PROCESS(
      COMMAND ${PKG_CONFIG_EXECUTABLE} --variable=targets gdk-3.0
      OUTPUT_VARIABLE TARGET_RESULT
      OUTPUT_STRIP_TRAILING_WHITESPACE
  )
  IF(TARGET_RESULT STREQUAL "quartz")
    SET(GNC_PLATFORM_COCOA 1)
    SET(GDK_QUARTZ 1)
    GNC_PKG_CHECK_MODULES(GTK_MAC gtk-mac-integration-gtk3)
    IF (GTK_MAC_FOUND)
      SET(MAC_INTEGRATION 1)
      FIND_LIBRARY(COCOA_LIBRARY Cocoa)
      FIND_LIBRARY(SECURITY_LIBRARY Security)
      FIND_LIBRARY(CARBON_LIBRARY Carbon)
      SET(OSX_EXTRA_COMPILE_FLAGS $<$<COMPILE_LANGUAGE:C>:-xobjective-c> $<$<COMPILE_LANGUAGE:CXX>:-xobjective-c++>)
      SET(OSX_EXTRA_LIBRARIES objc ${COCOA_LIBRARY} ${SECURITY_LIBRARY} ${CARBON_LIBRARY})
    ENDIF(GTK_MAC_FOUND)
  ENDIF()
ENDIF(APPLE)

# FIND_PACKAGE(LibXslt) eats PKG_CONFIG_EXECUTABLE, so preserve it.
SET(GNC_PKG_CONFIG_EXE ${PKG_CONFIG_EXECUTABLE})
# ############################################################

# xsltproc
FIND_PACKAGE(LibXslt)

IF (NOT LIBXSLT_FOUND)
   MESSAGE(FATAL_ERROR "libxslt library not found.")
ENDIF(NOT LIBXSLT_FOUND)
if (${LIBXSLT_XSLTPROC_EXECUTABLE} STREQUAL "LIBXSLT_XSLTPROC_EXECUTABLE-NOTFOUND")
  message(FATAL_ERROR "xsltproc executable not found")
endif ()
# ############################################################
SET(PKG_CONFIG_EXECUTABLE ${GNC_PKG_CONFIG_EXE})

# libdbi
FIND_PATH (LIBDBI_INCLUDE_PATH dbi/dbi.h)
FIND_LIBRARY (LIBDBI_LIBRARY dbi)
FIND_LIBRARY (LIBDBI_DRIVERS_DIR
  NAMES dbdmysql dbdpgsql dbdsqlite3 NAMES_PER_DIR
  PATH_SUFFIXES dbd libdbi-drivers/dbd
  HINTS LIBDBI_LIBRARY
  PATHS GNC_DBD_DIR
  DOC "Libdbi Drivers Directory")
IF (WITH_SQL)
  IF (NOT LIBDBI_INCLUDE_PATH)
    MESSAGE (SEND_ERROR "Include file <dbi/dbi.h> was not found - did you install libdbi0-dev or libdbi-dev?")
  ENDIF (NOT LIBDBI_INCLUDE_PATH)
  IF (NOT LIBDBI_LIBRARY)
    MESSAGE (SEND_ERROR "Library libdbi was not found")
  ENDIF (NOT LIBDBI_LIBRARY)
  SET(HAVE_DBI_DBI_H 1)
  IF (NOT LIBDBI_DRIVERS_DIR)
    MESSAGE (SEND_ERROR "No libdbi drivers found, SQL tests will fail.")
  ELSE()
    get_filename_component(drivers_dir ${LIBDBI_DRIVERS_DIR} DIRECTORY)
    set(LIBDBI_DRIVERS_DIR ${drivers_dir} CACHE FILEPATH "Directory containing the libdbi driver modules." FORCE)
  ENDIF()
ENDIF (WITH_SQL)

# ############################################################

IF (WITH_PYTHON)
  FIND_PACKAGE(PythonInterp 3)
  IF (NOT PYTHONINTERP_FOUND)
    MESSAGE(SEND_ERROR "Python support enabled, but Python interpreter not found.")
  ENDIF()

  IF (PYTHON_VERSION_STRING VERSION_LESS "2.4.0")
    MESSAGE(SEND_ERROR "Found python version ${PYTHON_VERSION_STRING}, but it's too old. Need python >= 2.4.0")
  ENDIF()

  FIND_PACKAGE(PythonLibs 3)
  IF (NOT PYTHONLIBS_FOUND)
    MESSAGE(SEND_ERROR "Python support enabled, but Python libraries not found.")
  ENDIF()

  # Determine where to install the python libraries.
  EXECUTE_PROCESS(
    COMMAND ${PYTHON_EXECUTABLE} -c "from distutils import sysconfig; print(sysconfig.get_python_lib(prefix='${CMAKE_INSTALL_PREFIX}', plat_specific=True))"
    RESULT_VARIABLE PYTHON_SYSCONFIG_RESULT
    OUTPUT_VARIABLE PYTHON_SYSCONFIG_OUTPUT
    ERROR_VARIABLE PYTHON_SYSCONFIG_ERROR
    OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_STRIP_TRAILING_WHITESPACE
  )
  IF (PYTHON_SYSCONFIG_RESULT)
    MESSAGE(SEND_ERROR "Could not determine Python site-package directory:\n${PYTHON_SYSCONFIG_ERROR}")
  ENDIF()
  STRING(REPLACE ${CMAKE_INSTALL_PREFIX} ${CMAKE_BINARY_DIR} PYTHON_SYSCONFIG_BUILD ${PYTHON_SYSCONFIG_OUTPUT})  
ENDIF()



FIND_PACKAGE(Perl)

if (NOT PERL_FOUND)
  MESSAGE(SEND_ERROR "Perl executable not found. Please set PERL_EXECUTABLE.")
ENDIF(NOT PERL_FOUND)

GET_FILENAME_COMPONENT(PERL_DIR ${PERL_EXECUTABLE} DIRECTORY)

FIND_PROGRAM(POD2MAN_EXECUTABLE pod2man HINTS ${PERL_DIR})

#ICU
GNC_PKG_CHECK_MODULES (ICU4C REQUIRED icu-uc)
GNC_PKG_CHECK_MODULES (ICU4C_I18N REQUIRED icu-i18n)

#BOOST
SET (Boost_USE_MULTITHREADED ON)
SET (Boost_FIND_QUIETLY ON)

IF (NOT DEFINED ${BOOST_ROOT})
  SET(BOOST_ROOT $ENV{BOOST_ROOT})
ENDIF()
FIND_PACKAGE (Boost 1.54.0 REQUIRED COMPONENTS date_time regex locale filesystem system)

IF (Boost_FOUND)
  include_directories(${Boost_INCLUDE_DIRS})
  SET(HAVE_BOOST 1)
ELSE (Boost_FOUND)
  MESSAGE (SEND_ERROR "Boost 1.54.0 or later is not installed, and is required. Please install it and ensure that the following libraries are built: chrono, date_time, filesystem, log, program_options, regex, signals, system, and test.")
ENDIF (Boost_FOUND)


# Compiler flags

if (APPLE)
  include (CheckCCompilerFlag)
  include (CheckCxxCompilerFlag)
  Check_CXX_Compiler_Flag(-Wno-unused-local-typedef, have_wno_ult)
  if (have_wno_ult)
    SET( CMAKE_CXX_FLAGS "-Wno-unused-local-typedef -Wmissing-prototypes ${CMAKE_CXX_FLAGS}")
  else()
    SET( CMAKE_CXX_FLAGS "-Wmissing-prototypes ${CMAKE_CXX_FLAGS}")
  endif()
  Check_C_Compiler_Flag(-Wno-unknown-attributes have_no_unkatt)
  if (have_no_unkatt)
    set(CMAKE_C_FLAGS "-Wno-unknown-attributes -Wno-typedef-redefinition ${CMAKE_C_FLAGS}")
    set(CMAKE_CXX_FLAGS "-Wno-unknown-attributes -Wno-typedef-redefinition ${CMAKE_CXX_FLAGS}")
  else()
    set(CMAKE_C_FLAGS "-Wno-typedef-redefinition ${CMAKE_C_FLAGS}")
    set(CMAKE_CXX_FLAGS "-Wno-typedef-redefinition ${CMAKE_CXX_FLAGS}")
  endif()
  #  SET (CMAKE_OSX_ARCHITECTURES "i386")
endif (APPLE)


# Also, set the C++ version to c++11
SET(CMAKE_CXX_FLAGS "-std=gnu++11 ${CMAKE_CXX_FLAGS}")  # FIXME: should be -std=c++11


IF (UNIX)
  SET( CMAKE_C_FLAGS "-Werror -Wdeclaration-after-statement -Wno-pointer-sign -Wall -Wmissing-prototypes -Wmissing-declarations -Wno-unused ${CMAKE_C_FLAGS}")
  SET( CMAKE_CXX_FLAGS "-Werror -Wall -Wmissing-declarations -Wno-unused ${CMAKE_CXX_FLAGS}")
  SET( CMAKE_C_FLAGS "-Wno-deprecated-declarations -std=gnu11 ${CMAKE_C_FLAGS}")
  SET( CMAKE_C_FLAGS_RELEASE "-O3 -U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=2 ${CMAKE_C_FLAGS}")
ENDIF (UNIX)
IF (MINGW)
  SET( CMAKE_C_FLAGS "-Werror -Wdeclaration-after-statement -Wno-pointer-sign -Wall -Wunused -Wmissing-prototypes -Wmissing-declarations  -Wno-unused -Wno-error=deprecated-declarations -std=gnu11 ${CMAKE_C_FLAGS}")
  SET( CMAKE_CXX_FLAGS "-DWINVER=0x0500 -D_EMULATE_GLIBC=0 ${CMAKE_CXX_FLAGS}") # Workaround for bug in gtest on mingw, see https://github.com/google/googletest/issues/893 and https://github.com/google/googletest/issues/920
ENDIF (MINGW)

IF (APPLE AND WITH_GNUCASH)
  SET(CMAKE_MACOSX_RPATH ON)
  SET(CMAKE_INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}")
ENDIF (APPLE AND WITH_GNUCASH)

IF (UNIX)
  SET(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_FULL_LIBDIR}:${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
ENDIF()

SET(BUILD_SHARED_LIBS ON)

# Most libraries are installed to lib/gnucash, so set that as the default.
# For the handful that are installed to lib, we override the properies below
# (after the targets have been read in).


SET(CMAKE_LIBRARY_OUTPUT_DIRECTORY         ${LIBDIR_BUILD}/gnucash)
IF (XCODE_VERSION)
  # FIXME: These settings break the ability to manipulate different configurations (Debug,
  # FIXME: Release, etc.) in Xcode. We'll need to change the module loading C code if
  # we want to support multi config.
  SET(CMAKE_LIBRARY_OUTPUT_DIRECTORY_DEBUG   ${LIBDIR_BUILD}/gnucash)
  SET(CMAKE_LIBRARY_OUTPUT_DIRECTORY_RELEASE ${LIBDIR_BUILD}/gnucash)
ENDIF()

# For binarines
SET(CMAKE_RUNTIME_OUTPUT_DIRECTORY         ${CMAKE_BINARY_DIR}/bin)
IF (XCODE_VERSION)
  SET(CMAKE_RUNTIME_OUTPUT_DIRECTORY_DEBUG   ${CMAKE_BINARY_DIR}/bin)
  SET(CMAKE_RUNTIME_OUTPUT_DIRECTORY_RELEASE ${CMAKE_BINARY_DIR}/bin)
ENDIF()
# ############################################################

GNC_GTEST_CONFIGURE()

# There are targets that need to build before tests will run
ADD_CUSTOM_TARGET(check
  COMMAND ${CMAKE_CTEST_COMMAND}
)

SET(gnucash_DOCS
    AUTHORS
    ChangeLog.1999
    ChangeLog.2000
    ChangeLog.2001
    ChangeLog.2002
    ChangeLog.2003
    ChangeLog.2004
    ChangeLog.2005
    ChangeLog.2006
    ChangeLog.2007
    ChangeLog.2008
    ChangeLog.2009
    ChangeLog.2010
    ChangeLog.2011
    ChangeLog.2012
    ChangeLog.2013
    ChangeLog.2014
    ChangeLog.2015
    ChangeLog.2016
    ChangeLog.2017
    DOCUMENTERS
    HACKING
    LICENSE
    NEWS
    README.dependencies
)


INSTALL(FILES ${gnucash_DOCS} DESTINATION ${CMAKE_INSTALL_DOCDIR})

# ############################################################

# Create config.h

SET (GETTEXT_PACKAGE "gnucash")

IF (WIN32)
IF (MINGW)
SET (HAVE_SCANF_LLD 1)
ELSE ()
SET (HAVE_SCANF_I64D 1)
ENDIF ()
SET (HAVE_HTMLHELPW 1)
ENDIF (WIN32)

CHECK_INCLUDE_FILES (dirent.h HAVE_DIRENT_H)
CHECK_INCLUDE_FILES (dlfcn.h HAVE_DLFCN_H)
CHECK_INCLUDE_FILES (glob.h HAVE_GLOB_H)
CHECK_INCLUDE_FILES (inttypes.h HAVE_INTTYPES_H)
CHECK_INCLUDE_FILES (limits.h HAVE_LIMITS_H)
CHECK_INCLUDE_FILES (locale.h HAVE_LOCALE_H)
CHECK_INCLUDE_FILES (memory.h HAVE_MEMORY_H)
CHECK_INCLUDE_FILES (stdint.h HAVE_STDINT_H)
CHECK_INCLUDE_FILES (stdlib.h HAVE_STDLIB_H)
CHECK_INCLUDE_FILES (string.h HAVE_STRING_H)
CHECK_INCLUDE_FILES (strings.h HAVE_STRINGS_H)
CHECK_INCLUDE_FILES (sys/stat.h HAVE_SYS_STAT_H)
CHECK_INCLUDE_FILES (sys/time.h HAVE_SYS_TIME_H)
CHECK_INCLUDE_FILES (sys/times.h HAVE_SYS_TIMES_H)
CHECK_INCLUDE_FILES (sys/types.h HAVE_SYS_TYPES_H)
CHECK_INCLUDE_FILES (sys/wait.h HAVE_SYS_WAIT_H)
CHECK_INCLUDE_FILES (unistd.h HAVE_UNISTD_H)
CHECK_INCLUDE_FILES (utmp.h HAVE_UTMP_H)
CHECK_INCLUDE_FILES (wctype.h HAVE_WCTYPE_H)

IF (NOT DISABLE_NLS)
SET(ENABLE_NLS 1)
ENDIF(NOT DISABLE_NLS)

IF (ENABLE_BINRELOC)
IF (UNIX OR MINGW)
SET(BR_PTHREAD 1)
ENDIF(UNIX OR MINGW)
ENDIF(ENABLE_BINRELOC)

IF (UNIX OR MINGW)
SET (HAVE_BIND_TEXTDOMAIN_CODESET 1)
SET (HAVE_DCGETTEXT 1)
SET (HAVE_GETTEXT 1)
SET (HAVE_GETTIMEOFDAY 1)
SET (HAVE_GUILE 1)
SET (HAVE_LIBM 1)
SET (HAVE_MEMCPY 1)
SET (STDC_HEADERS 1)
SET (_ALL_SOURCE 1)
SET (_GNU_SOURCE 1)
SET (_POSIX_PTHREAD_SEMANTICS 1)
SET (_TANDEM_SOURCE 1)
SET (__EXTENSIONS__ 1)
ENDIF (UNIX OR MINGW)

IF (UNIX)
SET (HAVE_CHOWN 1)
SET (HAVE_DLERROR 1)
SET (HAVE_DLSYM 1)
SET (HAVE_GETHOSTID 1)
SET (HAVE_GETHOSTNAME 1)
SET (HAVE_GETPPID 1)
SET (HAVE_GETUID 1)
SET (HAVE_GMTIME_R 1)
SET (HAVE_LANGINFO_D_FMT 1)
SET (HAVE_LC_MESSAGES 1)
SET (HAVE_LIBPTHREAD 1)
SET (HAVE_LINK 1)
SET (HAVE_LOCALTIME_R 1)
SET (HAVE_PTHREAD_MUTEX_INIT 1)
SET (HAVE_PTHREAD_PRIO_INHERIT 1)
SET (HAVE_SCANF_LLD 1)
SET (HAVE_SETENV 1)
SET (HAVE_STPCPY 1)
SET (HAVE_STRFMON 1)
SET (HAVE_STRPTIME 1)
SET (HAVE_STRUCT_TM_GMTOFF 1)
SET (HAVE_TIMEGM 1)
SET (HAVE_TOWUPPER 1)
SET (GNC_PLATFORM_POSIX 1)
ENDIF (UNIX)

IF (WIN32)
SET (GNC_PLATFORM_WINDOWS 1)
ENDIF (WIN32)

IF (APPLE)
# FIXME: HANDLE gtk-mac-integration-gtk2
SET(GNC_PLATFORM_DARWIN 1)
SET(GNC_PLATFORM_OSX 1)
SET(PLATFORM_OSX 1)
SET(HAVE_OSX_KEYCHAIN 1)
ENDIF(APPLE)

IF(GLIB2_VERSION VERSION_GREATER 2.46.0 OR GLIB2_VERSION VERSION_EQUAL 2.46.0)
SET(HAVE_GLIB_2_46 1)
ENDIF()

IF(DISABLE_DEPRECATED_GNOME)
SET(GNOME_DISABLE_DEPRECATED 1)
ENDIF(DISABLE_DEPRECATED_GNOME)

IF(DISABLE_DEPRECATED_GTK)
SET(GTK_DISABLE_DEPRECATED 1)
SET(GDK_DISABLE_DEPRECATED 1)
SET(GDK_PIXMAP_DISABLE_DEPRECATED 1)
ENDIF(DISABLE_DEPRECATED_GTK)

IF(DISABLE_DEPRECATED_GLIB)
SET(G_DISABLE_DEPRECATED 1)
ENDIF(DISABLE_DEPRECATED_GLIB)

ADD_DEFINITIONS (-DHAVE_CONFIG_H)

SET (CONFIG_H ${CMAKE_CURRENT_BINARY_DIR}/common/config.h)
CONFIGURE_FILE (${CMAKE_CURRENT_SOURCE_DIR}/common/config.h.cmake.in ${CONFIG_H})

SET(SCHEME_INSTALLED_SOURCE_DIR ${CMAKE_INSTALL_DATADIR}/gnucash/scm)
SET(SCHEME_INSTALLED_CACHE_DIR ${PKGLIBDIR}/scm/ccache/${GUILE_EFFECTIVE_VERSION})

# The subdirectories
ADD_SUBDIRECTORY (borrowed)
ADD_SUBDIRECTORY (data)
ADD_SUBDIRECTORY (doc)
ADD_SUBDIRECTORY (po)
ADD_SUBDIRECTORY (common)
ADD_SUBDIRECTORY (libgnucash)
IF (WITH_GNUCASH)
  ADD_SUBDIRECTORY (gnucash)
ENDIF (WITH_GNUCASH)
ADD_SUBDIRECTORY (bindings)
ADD_SUBDIRECTORY (test-templates)
ADD_SUBDIRECTORY (util)

# This cmake subdir must be the last add_subdirectory() call because
# it contains post-install actions to execute.
ADD_SUBDIRECTORY(cmake)

# Generate the ChangeLog

if (BUILDING_FROM_VCS)
    add_custom_target(ChangeLog ALL
        COMMAND ${GIT_EXECUTABLE} log --format=\"%ad %aN %n%n%x09* %s%d%n\" --date=short --since=2018-01-01 > ${CMAKE_BINARY_DIR}/ChangeLog
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    )
    install(FILES ${CMAKE_BINARY_DIR}/ChangeLog DESTINATION ${CMAKE_INSTALL_DOCDIR})
else()
    install(FILES ${CMAKE_SOURCE_DIR}/ChangeLog DESTINATION ${CMAKE_INSTALL_DOCDIR})
endif()
dist_add_generated (${BUILDING_FROM_VCS} ChangeLog)

############################ BEGIN MAKE DIST #################

SET(PACKAGE_PREFIX "${PACKAGE}-${PACKAGE_VERSION}")
SET(DIST_FILE "${PACKAGE_PREFIX}.tar")

SET(toplvl_DIST_local ${gnucash_DOCS}
  CMakeLists.txt README)


SET_LOCAL_DIST(toplvl_DIST ${toplvl_DIST_local})

# Each subdirectory is responsible for reporting its list of distribution files to its parent, up to here.
#
# Leaf directories use the SET_DIST_LIST() function to report their distribution files. See
# the bottom of libgnucash/app-utils/test/CMakeLists.txt for an example.
#
# A non leaf directories uses the SET_LOCAL_DIST() function to specify its distribution files local that dir,
# then uses a (SET ${foo_DIST} ${locals....} PARENT_SCOPE) command to report up. See the bottom of
# libgnucash/app-utils/CMakeLists.txt for an example of this.

SET(ALL_DIST ${bindings_DIST} ${borrowed_DIST} ${cmake_DIST} ${common_DIST} ${data_DIST}
    ${doc_DIST} ${gnucash_DIST} ${libgnucash_DIST} ${packaging_DIST}
    ${po_DIST} ${test_templates_DIST} ${toplvl_DIST} ${util_DIST})


IF (BUILDING_FROM_VCS)
  SET(BUILD_SOURCE_DIR ${CMAKE_BINARY_DIR})
ELSE()
  SET(BUILD_SOURCE_DIR ${CMAKE_SOURCE_DIR})
ENDIF()

# Write a dist manifest
STRING(REPLACE ";" "\n" ALL_DIST_LINES "${ALL_DIST}")
FILE(WRITE ${CMAKE_BINARY_DIR}/dist_manifest.txt ${ALL_DIST_LINES})

ADD_CUSTOM_COMMAND(OUTPUT ${DIST_FILE}.gz ${DIST_FILE}.bz2
        COMMAND ${CMAKE_COMMAND}
           -D CMAKE_MODULE_PATH=${CMAKE_SOURCE_DIR}/common/cmake_modules
           -D PACKAGE_PREFIX=${PACKAGE_PREFIX}
           -D GNUCASH_SOURCE_DIR=${CMAKE_SOURCE_DIR}
           -D BUILD_SOURCE_DIR=${BUILD_SOURCE_DIR}
           -D BUILDING_FROM_VCS=${BUILDING_FROM_VCS}
           -D SHELL=${SHELL}
           -D WITH_GNUCASH=${WITH_GNUCASH}
           "-Ddist_generated=\"${dist_generated}\""
           -P ${CMAKE_SOURCE_DIR}/common/cmake_modules/MakeDist.cmake

        DEPENDS
          ${ALL_DIST} ${dist_generated_depends}
        )

ADD_CUSTOM_TARGET(dist DEPENDS ${DIST_FILE}.gz ${DIST_FILE}.bz2)

ADD_CUSTOM_TARGET(distcheck DEPENDS dist
        COMMAND ${CMAKE_COMMAND}
            -D CMAKE_MODULE_PATH=${CMAKE_SOURCE_DIR}/common/cmake_modules
            -D CMAKE_PREFIX_PATH=${CMAKE_PREFIX_PATH}
            -D PACKAGE_PREFIX=${PACKAGE_PREFIX}
            -D CMAKE_C_FLAGS=${CMAKE_C_FLAGS}
            -D CMAKE_CXX_FLAGS=${CMAKE_CXX_FLAGS}
            -D GTEST_ROOT=${GTEST_ROOT}
            -D GMOCK_ROOT=${GMOCK_ROOT}
            -P ${CMAKE_SOURCE_DIR}/common/cmake_modules/MakeDistCheck.cmake
        )

############################# END MAKE DIST #################

# uninstall target
configure_file(
        "${CMAKE_SOURCE_DIR}/cmake/cmake_uninstall.cmake.in"
        "${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake"
        @ONLY)

add_custom_target(uninstall
        COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake)

SET(_MODULES gnc-core-utils gnc-module gnc-backend-xml-utils)

IF (WITH_SQL)
  LIST(APPEND _MODULES gnc-backend-sql)
ENDIF(WITH_SQL)

IF (WITH_GNUCASH)
  LIST(APPEND _MODULES gnc-gnome)
ENDIF(WITH_GNUCASH)

SET_TARGET_PROPERTIES(${_MODULES} PROPERTIES
  LIBRARY_OUTPUT_DIRECTORY         ${LIBDIR_BUILD}
  IF (XCODE_VERSION)
    LIBRARY_OUTPUT_DIRECTORY_DEBUG   ${LIBDIR_BUILD}
    LIBRARY_OUTPUT_DIRECTORY_RELEASE ${LIBDIR_BUILD}
  ENDIF()

  RUNTIME_OUTPUT_DIRECTORY         ${CMAKE_BINARY_DIR}/bin
  IF (XCODE_VERSION)
    RUNTIME_OUTPUT_DIRECTORY_DEBUG   ${CMAKE_BINARY_DIR}/bin
    RUNTIME_OUTPUT_DIRECTORY_RELEASE ${CMAKE_BINARY_DIR}/bin
  ENDIF()
)

IF (WIN32)
  SET_TARGET_PROPERTIES(gncmod-backend-xml gncmod-stylesheets PROPERTIES
    LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
  )
ENDIF()

#Link LICENSE to COPYING so that people expecting to find it,
#will. COPYING is normally linked by autogen.sh to the latest
#automake-provided version.
SET (_CMD create_symlink)
IF (WIN32)
  SET(_CMD copy)
ENDIF()

INSTALL(CODE
" EXECUTE_PROCESS(
      COMMAND ${CMAKE_COMMAND} -E ${_CMD} LICENSE COPYING
      WORKING_DIRECTORY ${CMAKE_INSTALL_DOCDIR}
  )"
)

#For windows, copy in some DLLs from Mingw

IF (WIN32)
  FIND_LIBRARY(LIBSTDC++ libstdc++-6.dll)
  FIND_LIBRARY(LIBDW2 libgcc_s_dw2-1.dll)
  SET(MINGW_DLLS ${LIBSTDC++} ${LIBDW2})
  INSTALL(PROGRAMS ${MINGW_DLLS} DESTINATION ${CMAKE_INSTALL_BINDIR})
  FILE(COPY ${MINGW_DLLS} DESTINATION ${BINDIR_BUILD}
    # Do permissions matter for windows?
    FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
  )
ENDIF()
# ############################################################
# Package creation rules

SET(CPACK_PACKAGE_VERSION_MAJOR "${GNUCASH_MAJOR_VERSION}")
SET(CPACK_PACKAGE_VERSION_MINOR "${GNUCASH_MINOR_VERSION}")
SET(CPACK_PACKAGE_VERSION_PATCH "${GNUCASH_MICRO_VERSION}")
SET(CPACK_PACKAGE_VERSION "${VERSION}")

IF(UNIX)
  SET(CPACK_GENERATOR "TGZ")
ENDIF(UNIX)

INCLUDE (CPack)
