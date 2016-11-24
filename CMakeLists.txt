# CMakeLists.txt for the cutecash sub-project of gnucash

# This is very EXPERIMENTAL! We will see how far this project will
# get.

IF (WIN32)
  CMAKE_MINIMUM_REQUIRED (VERSION 3.3.2)
ELSE()
  CMAKE_MINIMUM_REQUIRED (VERSION 3.0)
ENDIF()

PROJECT (Gnucash)

ENABLE_TESTING()

# Version number of gnucash
SET (GNUCASH_MAJOR_VERSION 2)
SET (GNUCASH_MINOR_VERSION 6)
SET (GNUCASH_MICRO_VERSION 99)
SET (GNUCASH_NANO_VERSION 0)
SET (VERSION "${GNUCASH_MAJOR_VERSION}.${GNUCASH_MINOR_VERSION}.${GNUCASH_MICRO_VERSION}")
SET (GNUCASH_LATEST_STABLE_SERIES 2.6)

SET (PACKAGE gnucash)
SET (PACKAGE_NAME GnuCash)
SET (PACKAGE_VERSION 2.6.99)
SET (PACKAGE_BUGREPORT gnucash-devel@gnucash.org)
SET (PACKAGE_TARNAME ${PACKAGE})
SET (PACKAGE_STRING "${PACKAGE_NAME} ${PACKAGE_VERSION}")
SET (PACKAGE_URL "")

# Change this in development versions when changing anything that
# affects stored data structures. Reset to zero when bumping version.

SET (GNUCASH_RESAVE_VERSION "19920")

SET(GETTEXT_PACKAGE "gnucash")

# Extra cmake macros
SET (CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/src/cmake_modules;${CMAKE_MODULE_PATH}")
INCLUDE (MacroAppendForeach)
INCLUDE (MacroAddSourceFileCompileFlags)
INCLUDE (GncAddSwigCommand)
INCLUDE (CheckIncludeFiles)
INCLUDE (GncAddSchemeTargets)
INCLUDE (GncConfigure)
INCLUDE (GncAddGSchemaTargets)
INCLUDE (GncAddTest)

# ############################################################
# These options are settable from the CMake command line. For example, to disable
# SQL, put -D WITH_SQL=OFF on the command line.

OPTION (WITH_SQL "Build this project with SQL (libdbi) support" ON)
OPTION (WITH_AQBANKING "Build this project with aqbanking (online banking) support" ON )
OPTION (WITH_GNUCASH "Build all of GnuCash, not just cutecash" ON)
OPTION (WITH_CUTECASH "Also build cutecash" OFF)
OPTION (WITH_OFX "compile with ofx support (needs LibOFX)" ON)
OPTION (WITH_PYTHON "enable python plugin and bindings" OFF)
OPTION (ENABLE_BINRELOC "compile with binary relocation support" ON)
OPTION (ENABLE_DEBUG "compile with debugging flags set" OFF)
OPTION (ENABLE_REGISTER2 "compile with register2 enabled" OFF)
OPTION (DISABLE_NLS "do not use Native Language Support" OFF)
OPTION (DISABLE_DEPRECATED_GLIB "don't use deprecated glib funcions" OFF)
OPTION (DISABLE_DEPRECATED_GTK "don't use deprectacted gtk, gdk or gdk-pixbuf functions" OFF)
OPTION (DISABLE_DEPRECATED_GNOME "don't use deprecated gnome functions" OFF)
OPTION (GNC_BUILD_AS_INSTALL "Make build directory structure mirror install" ON)
# ############################################################

# These are also settable from the command line in a similar way.

SET(BINDIR ${CMAKE_INSTALL_PREFIX}/bin CACHE STRING "user executables")
SET(SYSCONFDIR ${CMAKE_INSTALL_PREFIX}/etc CACHE STRING "read-only single-machine data")
SET(DATAROOTDIR ${CMAKE_INSTALL_PREFIX}/share CACHE STRING "read-only arch.-independent data root")
SET(DATADIR ${DATAROOTDIR} CACHE STRING "read-only architecture-independent data")
SET(LIBDIR ${CMAKE_INSTALL_PREFIX}/lib CACHE STRING "object code libraries")
SET(LOCALEDIR ${DATAROOTDIR}/locale CACHE STRING "locale-dependent data")
SET(GNC_HELPDIR ${DATADIR} CACHE STRING "where to store help files")
SET(DATADIRNAME share)
SET(GNC_SYSTEM_XDG_DATA_DIRS /usr/local/share /usr/share)
SET(GNC_DBD_DIR ${CMAKE_PREFIX_PATH}/lib/dbd CACHE PATH "specify location of libdbi drivers")
SET(PKGLIBDIR ${CMAKE_INSTALL_PREFIX}/lib/gnucash)
SET(TEST_MYSQL_URL "" CACHE STRING "MySQL database URL for testing")
SET(TEST_PGSQL_URL "" CACHE STRING "PgSQL database URL for testing")
SET(HTMLHELP_DIR ${CMAKE_PREFIX_PATH}/hh CACHE PATH "Specify location of Win32 HTMLHELP library")

IF(GNC_BUILD_AS_INSTALL)
  SET(DATADIR_BUILD    ${CMAKE_BINARY_DIR}/${DATADIRNAME})
  SET(LIBDIR_BUILD     ${CMAKE_BINARY_DIR}/lib)
  SET(SYSCONFDIR_BUILD ${CMAKE_BINARY_DIR}/etc)
  SET(BINDIR_BUILD     ${CMAKE_BINARY_DIR}/bin)

ENDIF()

SET(SHELL_FROM_ENV $ENV{SHELL})
SET(SHELL /bin/bash)
IF (WIN32)
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
ENDIF()

IF (WIN32)
  # Help Windows find the various dependencies. We assume here that the standard advice for building
  # GnuCash has been followed and that these dependencies live underneath something like C:/GCDEV, which
  # should be provided as CMAKE_PREFIX_PATH on the CMake command line:
  #      cmake -D CMAKE_PREFIX_PATH=c/gcdev -G "MSYS Makefiles" path/to/gnucash/sources
  #

  SET(CMAKE_FRAMEWORK_PATH_TMP ${CMAKE_PREFIX_PATH})
  SET(DEV_SUBDIRS aqbanking gnome goffice guile gwenhywfar libgsf libofx libsoup libxslt webkit)
  FOREACH(subdir ${DEV_SUBDIRS})
    LIST(APPEND CMAKE_FRAMEWORK_PATH_TMP ${CMAKE_PREFIX_PATH}/${subdir})
  ENDFOREACH()
  # Setting CMAKE_FRAMEWORK_PATH, which is intended for OS X, is a terrible hack here. But I don't
  # want to mess with CMAKE_PREFIX_PATH.
  SET(CMAKE_FRAMEWORK_PATH "${CMAKE_FRAMEWORK_PATH_TMP}")
  SET(PKG_CONFIG_EXECUTABLE ${CMAKE_PREFIX_PATH}/gnome/bin/pkg-config.exe)
  SET(SWIG_EXECUTABLE ${CMAKE_PREFIX_PATH}/swig/swig.exe)
  SET(BOOST_ROOT ${CMAKE_PREFIX_PATH}/boost)
  SET(REGEX_INCLUDE_PATH ${CMAKE_PREFIX_PATH}/regex/include)
  SET(REGEX_LDFLAGS "-L${CMAKE_PREFIX_PATH}/regex/lib -lregex")
  SET(LIBXSLT_INCLUDE_DIR ${CMAKE_PREFIX_PATH}/libxslt/include)
  SET(LIBXSLT_XSLTPROC_EXECUTABLE ${CMAKE_PREFIX_PATH}/libxslt/bin/xsltproc)
  # I have not yet debugged the intltool-merge file generation process on Windows.
  SET(NO_INTLTOOL TRUE)
#Prevent creating a console window on startup.
  SET(CMAKE_EXE_LINKER_FLAGS -mwindows)
ENDIF(WIN32)

IF (APPLE)
  SET (CMAKE_OSX_ARCHITECTURES "i386")
  ADD_DEFINITIONS(-arch i386)
  #SET(CMAKE_OSX_ARCHITECTURES "x86_64")
  # For PPC build, uncomment these lines (untested):
  #SET (CMAKE_OSX_ARCHITECTURES "i386;ppc")
  #ADD_DEFINITIONS(-arch i386 -arch ppc
  #SET (AC_APPLE_UNIVERSAL_BUILD 1)
ENDIF (APPLE)

FIND_PACKAGE(PkgConfig REQUIRED)

# The default FindPkgConfig.make code has a bug in how the setting of PKG_CONFIG_PATH is handled.
# The src/cmake_modules/GncFindPkgConfig.cmake file overrides a couple of macros in FindPkgConfig to fix.

INCLUDE (GncFindPkgConfig)

IF (NOT PKG_CONFIG_FOUND)
   MESSAGE (SEND_ERROR "pkg-config not found, but is required")
ENDIF (NOT PKG_CONFIG_FOUND)

# glib et al.
GNC_PKG_CHECK_MODULES (GLIB2 REQUIRED glib-2.0>=2.20)
GNC_PKG_CHECK_MODULES (GOBJECT REQUIRED gobject-2.0>=2.20)
GNC_PKG_CHECK_MODULES (GMODULE REQUIRED gmodule-2.0>=2.20)
GNC_PKG_CHECK_MODULES (GTHREAD REQUIRED gthread-2.0>=2.20)

GNC_PKG_CHECK_MODULES (LIBXML2 REQUIRED libxml-2.0>=2.7.0)
GNC_PKG_CHECK_MODULES (GNOMECANVAS REQUIRED libgnomecanvas-2.0)
GNC_PKG_CHECK_MODULES (LIBXSLT REQUIRED libxslt)
GNC_PKG_CHECK_MODULES (WEBKIT REQUIRED webkit-1.0>=1.2)
IF (WITH_GNUCASH)
  GNC_PKG_CHECK_MODULES (GTK2 REQUIRED gtk+-2.0>=2.24.0)
  GNC_PKG_CHECK_MODULES (GOFFICE REQUIRED libgoffice-0.8>=0.8.17)
ENDIF (WITH_GNUCASH)

GNC_PKG_CHECK_MODULES (ZLIB REQUIRED zlib)
IF (WITH_CUTECASH)
  GNC_PKG_CHECK_MODULES (GLIBMM REQUIRED glibmm-2.4>=2.24)
ENDIF(WITH_CUTECASH)

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



FIND_PROGRAM(INTLTOOL_MERGE NAMES intltool-merge HINTS ${CMAKE_PREFIX_PATH}/gnome/bin)
FIND_PROGRAM(GLIB_COMPILE_SCHEMAS glib-compile-schemas HINTS ${CMAKE_PREFIX_PATH}/gnome/bin)
FIND_PROGRAM(MSGFMT msgfmt)
IF(NOT INTLTOOL_MERGE)
  MESSAGE(SEND_ERROR "Can't find intltool-merge program. Please set INTLTOOL_MERGE.")
ENDIF(NOT INTLTOOL_MERGE)

IF (NOT GLIB_COMPILE_SCHEMAS)
  MESSAGE(SEND_ERROR "Can't find glib-compile-schemas program. Please set GLIB_COMPILE_SCHEMAS.")
ENDIF(NOT GLIB_COMPILE_SCHEMAS)

IF (NOT MSGFMT)
  MESSAGE(SEND_ERROR "Can't find msgfmt program. Please set MSGFMT.")
ENDIF(NOT MSGFMT)

FIND_PATH (REGEX_INCLUDE_PATH NAMES regex.h
		  PATHS /usr/include /opt/gnome/include)
FIND_LIBRARY (REGEX_LIBRARY NAMES regex)
IF (WIN32)
  FIND_PATH (HTMLHELP_INCLUDE_PATH NAMES htmlhelp.h HINTS ${HTMLHELP_DIR}/include)
  FIND_LIBRARY (HTMLHELP_LIBRARY htmlhelp HINTS ${HTMLHELP_DIR}/lib)
ENDIF (WIN32)

# ############################################################

# SWIG
IF(BUILDING_FROM_VCS)
  FIND_PACKAGE (SWIG REQUIRED)
  INCLUDE (${SWIG_USE_FILE})
ENDIF()

# Find Guile and determine which version we are using.
# First look for guile-2.0. If not found, try to locate guile-1.8

# guile library and include dir
GNC_PKG_CHECK_MODULES (GUILE2 guile-2.0>=2.0.9 QUIET)
IF (GUILE2_FOUND) # found guile-2.0
  ADD_DEFINITIONS (-DHAVE_GUILE20)
  SET(HAVE_GUILE2 TRUE)
  SET(GUILE_EFFECTIVE_VERSION 2.0)
  SET(GUILE_INCLUDE_DIRS ${GUILE2_INCLUDE_DIRS})
  SET(GUILE_LDFLAGS ${GUILE2_LDFLAGS})

  FIND_PROGRAM (GUILD_EXECUTABLE guild)
  IF (NOT GUILD_EXECUTABLE)
    MESSAGE (SEND_ERROR "The guild executable was not found, but is required. Please set GUILD_EXECUTABLE.")
  ENDIF (NOT GUILD_EXECUTABLE)
  MESSAGE(STATUS "Using guile-2.0.x")
ELSE()
  # look for guile 1.8
  GNC_PKG_CHECK_MODULES (GUILE1 guile-1.8>=1.8.8 QUIET)
  IF (NOT GUILE1_FOUND)
    MESSAGE (SEND_ERROR "Neither guile 1.8 nor guile 2.0 where find. Please set PKG_CONFIG_PATH one of these is found.")
  ENDIF(NOT GUILE1_FOUND)

  SET(HAVE_GUILE1 TRUE)
  SET(GUILE_EFFECTIVE_VERSION 1.8)
  SET(GUILE_INCLUDE_DIRS ${GUILE1_INCLUDE_DIRS})
  SET(GUILE_LDFLAGS ${GUILE1_LDFLAGS})
  MESSAGE(STATUS "Using guile-1.8.x")
ENDIF()

FIND_PROGRAM (GUILE_EXECUTABLE guile)
IF (NOT GUILE_EXECUTABLE)
  MESSAGE (SEND_ERROR "The guile executable was not found, but is required. Please set GUILE_EXECUTABLE.")
ENDIF (NOT GUILE_EXECUTABLE)

# Qt
IF (WITH_CUTECASH)
  SET (QT_MIN_VERSION "4.5.0")
  FIND_PACKAGE (Qt4 REQUIRED) # find and setup Qt4 for this project
ENDIF (WITH_CUTECASH)

# ############################################################
IF (WITH_AQBANKING)
  GNC_PKG_CHECK_MODULES (GWENHYWFAR REQUIRED gwenhywfar)
  GNC_PKG_CHECK_MODULES (AQBANKING REQUIRED aqbanking)
  IF(WITH_GNUCASH)
    GNC_PKG_CHECK_MODULES (GWENGUI_GTK2 REQUIRED gwengui-gtk2)
    GNC_PKG_CHECK_MODULES (KTOBLZCHECK ktoblzcheck)
    IF(KTOBLZCHECK_FOUND)
      SET(HAVE_KTOBLZCHECK_H 1)
    ENDIF(KTOBLZCHECK_FOUND)
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
      COMMAND ${PKG_CONFIG_EXECUTABLE} --variable=target gdk-2.0
      OUTPUT_VARIABLE TARGET_RESULT
      OUTPUT_STRIP_TRAILING_WHITESPACE
  )
  IF(TARGET_RESULT STREQUAL "quartz")
    SET(GNC_PLATFORM_COCOA 1)
    SET(GDK_QUARTZ 1)
    GNC_PKG_CHECK_MODULES(GTK_MAC gtk-mac-integration-gtk2)
    IF (GTK_MAC_FOUND)
      SET(MAC_INTEGRATION 1)
      FIND_LIBRARY(COCOA_LIBRARY Cocoa)
      FIND_LIBRARY(SECURITY_LIBRARY Security)
      FIND_LIBRARY(CARBON_LIBRARY Carbon)
      SET(OSX_EXTRA_COMPILE_FLAGS -xobjective-c)
      SET(OSX_EXTRA_LIBRARIES objc ${COCOA_LIBRARY} ${SECURITY_LIBRARY} ${CARBON_LIBRARY})
    ENDIF(GTK_MAC_FOUND)
  ENDIF()
ENDIF(APPLE)

# ############################################################

# xsltproc
FIND_PACKAGE(LibXslt)

IF (NOT LIBXSLT_FOUND)
   MESSAGE(SEND_ERROR "libxslt library not found.")
ENDIF(NOT LIBXSLT_FOUND)
# ############################################################

# libdbi
FIND_PATH (LIBDBI_INCLUDE_PATH dbi/dbi.h HINTS ${CMAKE_PREFIX_PATH}/libdbi/include)
FIND_LIBRARY (LIBDBI_LIBRARY dbi HINTS ${CMAKE_PREFIX_PATH}/libdbi/lib)
FIND_PATH (LIBDBI_DRIVERS_DIR dbd HINTS ${CMAKE_PREFIX_PATH}/lib ${CMAKE_PREFIX_PATH}/libdbi-drivers/lib)
IF (WITH_SQL)
  IF (NOT LIBDBI_INCLUDE_PATH)
    MESSAGE (SEND_ERROR "Include file <dbi/dbi.h> was not found - did you install libdbi0-dev or libdbi-dev?")
  ENDIF (NOT LIBDBI_INCLUDE_PATH)
  IF (NOT LIBDBI_LIBRARY)
    MESSAGE (SEND_ERROR "Library libdbi was not found")
  ENDIF (NOT LIBDBI_LIBRARY)
  SET(HAVE_DBI_DBI_H 1)
ENDIF (WITH_SQL)

# ############################################################

IF (WITH_PYTHON)
  FIND_PACKAGE(PythonInterp)
  IF (NOT PYTHONINTERP_FOUND)
    MESSAGE(SEND_ERROR "Python support enabled, but Python interpreter not found.")
  ENDIF()

  IF (PYTHON_VERSION_STRING VERSION_LESS "2.4.0")
    MESSAGE(SEND_ERROR "Found python version ${PYTHON_VERSION_STRING}, but it's too old. Need python >= 2.4.0")
  ENDIF()

  FIND_PACKAGE(PythonLibs)
  IF (NOT PYTHONLIBS_FOUND)
    MESSAGE(SEND_ERROR "Python support enabled, but Python libraries not found.")
  ENDIF()
ENDIF()



FIND_PACKAGE(Perl)

if (NOT PERL_FOUND)
  MESSAGE(SEND_ERROR "Perl executable not found. Please set PERL_EXECUTABLE.")
ENDIF(NOT PERL_FOUND)

GET_FILENAME_COMPONENT(PERL_DIR ${PERL_EXECUTABLE} DIRECTORY)

FIND_PROGRAM(POD2MAN_EXECUTABLE pod2man HINTS ${PERL_DIR})

#BOOST
IF (APPLE)
  # I suppose I would rather use the shared Boost libraries here, but the Boost install procedure
  # does not set RPATH for it's shared library correctly. It uses a 'naked' RPATH, which requires
  # that DYLD_FALLBACK_LIBRARY_PATH be set. There is supposedly a dll-path option so set RPATH, but
  # I can't get it to work (http://www.boost.org/build/doc/html/bbv2/faq/dll-path.html) and neither
  # can some other random person on the internet:
  # http://stackoverflow.com/questions/33667795/dll-path-has-no-effect-when-building-boost
  # Using static libs simplifies things.
  SET (Boost_USE_STATIC_LIBS ON)
  SET (Boost_USE_STATIC_RUNTIME ON)
ELSE()
  SET (Boost_USE_STATIC_LIBS OFF)
  SET (Boost_USE_STATIC_RUNTIME OFF)
ENDIF()

SET (Boost_USE_MULTITHREADED ON)
SET (Boost_FIND_QUIETLY ON)

FIND_PACKAGE (Boost 1.54.0 REQUIRED COMPONENTS date_time regex)

IF (Boost_FOUND)
  include_directories(${Boost_INCLUDE_DIRS})
  SET(HAVE_BOOST 1)
ELSE (Boost_FOUND)
  MESSAGE (SEND_ERROR "Boost 1.54.0 or later is not installed, and is required. Please install it and ensure that the following libraries are built: chrono, date_time, filesystem, log, program_options, regex, signals, system, and test.")
ENDIF (Boost_FOUND)


# Compiler flags

IF (APPLE)
  ADD_DEFINITIONS(-Wno-unknown-attributes -Wno-typedef-redefinition)
  #  SET (CMAKE_OSX_ARCHITECTURES "i386")
ENDIF (APPLE)


# Also, set the C++ version to c++11
SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=gnu++11")  # FIXME: should be -std=c++11


IF (UNIX)
  SET( CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Werror -Wdeclaration-after-statement -Wno-pointer-sign -Wall -Wunused -Wmissing-prototypes -Wmissing-declarations  -Wno-unused")
  SET( CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wno-error=deprecated-declarations -std=gnu99")
  SET( CMAKE_C_FLAGS_RELEASE "-O3 ${CMAKE_C_FLAGS} -U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=2")
ENDIF (UNIX)
IF (MINGW)
  SET( CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Werror -Wdeclaration-after-statement -Wno-pointer-sign -Wall -Wunused -Wmissing-prototypes -Wmissing-declarations  -Wno-unused -std=gnu99")
ENDIF (MINGW)
IF (APPLE)
  SET( CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-unused-local-typedef")
ENDIF(APPLE)
SET( CMAKE_C_FLAGS_DEBUG "-O0 -g ${CMAKE_C_FLAGS}")
SET( CMAKE_CXX_FLAGS_DEBUG "-O0 -g ${CMAKE_CXX_FLAGS}")

IF (APPLE AND WITH_GNUCASH)
  SET(CMAKE_MACOSX_RPATH ON)
ENDIF (APPLE AND WITH_GNUCASH)

IF (UNIX AND NOT APPLE)
  SET(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib:${CMAKE_INSTALL_PREFIX}/lib/gnucash")
ENDIF()

SET(BUILD_SHARED_LIBS ON)

IF (GNC_BUILD_AS_INSTALL)
  # Most libraries are installed to lib/gnucash, so set that as the default.
  # For the handful that are installed to lib, we override the properies below
  # (after the targets have been read in).


  SET(CMAKE_LIBRARY_OUTPUT_DIRECTORY         ${CMAKE_BINARY_DIR}/lib/gnucash)
  IF (XCODE_VERSION)
    # FIXME: These settings break the ability to manipulate different configurations (Debug,
    # FIXME: Release, etc.) in Xcode. We'll need to change the module loading C code if
    # we want to support multi config.
    SET(CMAKE_LIBRARY_OUTPUT_DIRECTORY_DEBUG   ${CMAKE_BINARY_DIR}/lib/gnucash)
    SET(CMAKE_LIBRARY_OUTPUT_DIRECTORY_RELEASE ${CMAKE_BINARY_DIR}/lib/gnucash)
  ENDIF()
  # For binarines
  SET(CMAKE_RUNTIME_OUTPUT_DIRECTORY         ${CMAKE_BINARY_DIR}/bin)
  IF (XCODE_VERSION)
    SET(CMAKE_RUNTIME_OUTPUT_DIRECTORY_DEBUG   ${CMAKE_BINARY_DIR}/bin)
    SET(CMAKE_RUNTIME_OUTPUT_DIRECTORY_RELEASE ${CMAKE_BINARY_DIR}/bin)
  ENDIF()
ENDIF()
# ############################################################

GNC_GTEST_CONFIGURE()

SET(CMAKE_CTEST_COMMAND ctest)
IF (XCODE_VERSION)
  SET(CMAKE_CTEST_COMMAND ctest -C Debug)
ENDIF()

# There are targets that need to build before tests will run
ADD_CUSTOM_TARGET(check
  COMMAND ${CMAKE_CTEST_COMMAND}
  DEPENDS gnucash scm-app-utils scm-gnome-utils scm-report-gnome
     scm-engine scm-gnc-module scm-test-engine scm-report-system-3
     scm-test-core scm-test-report-system scm-standard-reports-2
     scm-test-standard-reports foo gncmodfoo baz gncmodbaz
     bar gncmodbar gncmod_agedver gncmod_incompatdep
     gncmod_futuremodsys test-core-guile
)
IF (NOT WIN32)
  ADD_DEPENDENCIES(check gncmod-backend-xml-link)
ENDIF()

# The subdirectories
ADD_SUBDIRECTORY (accounts)
ADD_SUBDIRECTORY (checks)
ADD_SUBDIRECTORY (doc)
ADD_SUBDIRECTORY (packaging)
ADD_SUBDIRECTORY (po)
ADD_SUBDIRECTORY (src)

# This cmake subdir must be the last add_subdirectory() call because
# it contains post-install actions to execute.
ADD_SUBDIRECTORY(cmake)

IF (GNC_BUILD_AS_INSTALL)
  SET_TARGET_PROPERTIES(gnc-qof gnc-core-utils gnc-module gnc-backend-sql gnc-backend-xml-utils
    gnc-business-ledger gnc-gnome PROPERTIES
    LIBRARY_OUTPUT_DIRECTORY         ${CMAKE_BINARY_DIR}/lib
    IF (XCODE_VERSION)
      LIBRARY_OUTPUT_DIRECTORY_DEBUG   ${CMAKE_BINARY_DIR}/lib
      LIBRARY_OUTPUT_DIRECTORY_RELEASE ${CMAKE_BINARY_DIR}/lib
    ENDIF()

    RUNTIME_OUTPUT_DIRECTORY         ${CMAKE_BINARY_DIR}/bin
    IF (XCODE_VERSION)
      RUNTIME_OUTPUT_DIRECTORY_DEBUG   ${CMAKE_BINARY_DIR}/bin
      RUNTIME_OUTPUT_DIRECTORY_RELEASE ${CMAKE_BINARY_DIR}/bin
    ENDIF()
  )
  IF (WIN32)
    SET_TARGET_PROPERTIES(gncmod-backend-xml gncmod-stylesheets gncmod-utility-reports PROPERTIES
      LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
    )
  ENDIF()
ENDIF()
# Install DOC files from top level directory

SET(gnucash_DOCS
    AUTHORS
    ChangeLog
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
    DOCUMENTERS
    HACKING
    LICENSE
    NEWS
    README.dependencies
)

INSTALL(FILES ${gnucash_DOCS} DESTINATION share/doc/gnucash)

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
      WORKING_DIRECTORY ${CMAKE_INSTALL_PREFIX}/share/doc/gnucash
  )"
)

#For windows, copy in some DLLs from Mingw

IF (WIN32)
  SET(MINGW_DLLS ${CMAKE_PREFIX_PATH}/mingw/bin/libstdc++-6.dll ${CMAKE_PREFIX_PATH}/mingw/bin/libgcc_s_dw2-1.dll)
  INSTALL(PROGRAMS ${MINGW_DLLS} DESTINATION bin)
  IF (GNC_BUILD_AS_INSTALL)
    FILE(COPY ${MINGW_DLLS} DESTINATION ${BINDIR_BUILD}
      # Do permissions matter for windows?
      FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
    )
  ENDIF()


ENDIF()
# ############################################################
# Package creation rules

IF (WITH_CUTECASH)
SET (CUTECASH_VERSION_MAJOR "0")
SET (CUTECASH_VERSION_MINOR "1")
SET (CUTECASH_VERSION_PATCH "0")
SET (CUTECASH_VERSION_NANO "0")

SET (CPACK_PACKAGE_NAME "Cutecash")
SET (CPACK_PACKAGE_DESCRIPTION_SUMMARY "Cutecash Free Finance Software")
SET (CPACK_PACKAGE_VERSION_MAJOR ${CUTECASH_VERSION_MAJOR})
SET (CPACK_PACKAGE_VERSION_MINOR ${CUTECASH_VERSION_MINOR})
SET (CPACK_PACKAGE_VERSION_PATCH ${CUTECASH_VERSION_PATCH})
SET (CPACK_PACKAGE_VENDOR "Christian Stimming")
SET (CPACK_SOURCE_IGNORE_FILES "/\\\\.svn/;/\\\\.git/;.*~;build.*;html;Debug;Release")
SET (CPACK_STRIP_FILES "bin/cutecash")
#SET (CPACK_PACKAGE_EXECUTABLES "bin/carclient")
ENDIF (WITH_CUTECASH)
IF(UNIX)
  SET(CPACK_GENERATOR "TGZ")
ENDIF(UNIX)

INCLUDE (CPack)
