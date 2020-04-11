# CMakeLists.txt for GnuCash

cmake_minimum_required (VERSION 3.5)

project (gnucash
    VERSION 3.10
)

enable_testing()

# Version number of gnucash
set (GNUCASH_LATEST_STABLE_SERIES 3.x)

set (PACKAGE_NAME GnuCash)
set (PACKAGE_BUGREPORT "https://bugs.gnucash.org")
set (PACKAGE_STRING "${PACKAGE_NAME} ${PROJECT_VERSION}")
set (PACKAGE_URL "https://www.gnucash.org/")

# Change this in development versions when changing anything that
# affects stored data structures. Reset to zero when bumping version.

set (GNUCASH_RESAVE_VERSION "19920")

# Clear cache variables that will be filled later during the cmake run
unset(dist_generated CACHE)
unset(dist_generated_depends CACHE)

# Extra cmake macros
set (CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/common/cmake_modules;${CMAKE_MODULE_PATH}")
# CMake does a non-recursive build that puts the final build product directories in the build root. Some code needs to know this.

include (MacroAppendForeach)
include (MacroAddSourceFileCompileFlags)
include (GncAddSwigCommand)
include (CheckIncludeFiles)
include (GncAddSchemeTargets)
include (GncAddGSchemaTargets)
include (GncAddTest)
include (MakeDistFiles)
include (GNUInstallDirs)
include (TestBigEndian)

# ############################################################
# These options are settable from the CMake command line. For example, to disable
# SQL, put -D WITH_SQL=OFF on the command line.

option (WITH_SQL "Build this project with SQL (libdbi) support" ON)
option (WITH_AQBANKING "Build this project with aqbanking (online banking) support" ON)
option (WITH_GNUCASH "Build all of GnuCash, not just the library" ON)
option (WITH_OFX "compile with ofx support (needs LibOFX)" ON)
option (WITH_PYTHON "enable python plugin and bindings" OFF)
option (ENABLE_BINRELOC "compile with binary relocation support" ON)
option (ENABLE_REGISTER2 "compile with register2 enabled" OFF)
option (DISABLE_NLS "do not use Native Language Support" OFF)
option (DISABLE_DEPRECATED_GLIB "don't use deprecated glib functions" OFF)
option (DISABLE_DEPRECATED_GTK "don't use deprecated gtk, gdk or gdk-pixbuf functions" OFF)
option (DISABLE_DEPRECATED_GNOME "don't use deprecated gnome functions" OFF)
# ############################################################

# These are also settable from the command line in a similar way.

# Use gsettings-desktop-schemas for a better integration with GNOME
set(COMPILE_GSCHEMAS ON CACHE BOOL "Compile the GSettings schema")

# GENERATE_SWIG_WRAPPERS - Controls whether to generate the swig wrappers for guile and python. If not set the wrappers will only be generated when building from a git worktree, commented out here, but will be evaluated later on in this file

# Support for downstream packagers
set(GNUCASH_BUILD_ID "" CACHE STRING "Overrides the GnuCash build identification (Build ID) which defaults to a description of the vcs commit from which gnucash is built. Distributions may want to insert a package management based version number instead")
# In addition to the GNUCASH_BUILD_ID environment variable, packagers can overwrite
# gnucash/gnome/gnucash.releases.xml with (package) release details to be included
# in the appdata file. It should follow appdata <release> node conventions.


# Check that all of the absolute install paths are inside
# ${CMAKE_INSTALL_PREFIX}. If they're not, disable binreloc as it
# won't be able to find paths that aren't relative to the location of
# the executable.
foreach(install_dir ${CMAKE_INSTALL_FULL_BINDIR}
    ${CMAKE_INSTALL_FULL_SYSCONFDIR} ${CMAKE_INSTALL_FULL_DATAROOTDIR}
    ${CMAKE_INSTALL_FULL_DATADIR} ${CMAKE_INSTALL_FULL_LIBDIR})
  string(FIND ${install_dir} ${CMAKE_INSTALL_PREFIX} in_prefix)
  if(NOT (in_prefix EQUAL 0))
    set(ENABLE_BINRELOC OFF)
    message(WARNING "${install_dir} is set outside of the intallation prefix ${CMAKE_INSTALL_PREFIX}. That will break relocation so ENABLE_BINRELOC is set to off. With relocation disabled GnuCash will run only in its configured install location. You must set GNC_UNINSTALLED=1 and GNC_BUILDDIR=/path/to/builddir to run from the build directory. GnuCash will not run from a DESTDIR.")
    break()
  endif()
endforeach()

# GnuCash installs two files in ${CMAKE_INSTALL_SYSCONFDIR}
set(BINDIR ${CMAKE_INSTALL_BINDIR} CACHE STRING "user executables")
set(SYSCONFDIR ${CMAKE_INSTALL_SYSCONFDIR} CACHE STRING "read-only single-machine data")
set(DATAROOTDIR ${CMAKE_INSTALL_DATAROOTDIR} CACHE STRING "read-only arch.-independent data root")
set(DATADIR ${CMAKE_INSTALL_DATADIR} CACHE STRING "read-only architecture-independent data")
set(LIBDIR ${CMAKE_INSTALL_LIBDIR} CACHE STRING "object code libraries")
set(LOCALEDIR ${DATAROOTDIR}/locale CACHE STRING "locale-dependent data")
set(GNC_HELPDIR ${DATADIR} CACHE STRING "where to store help files")
set(DATADIRNAME share)
set(GNC_SYSTEM_XDG_DATA_DIRS /usr/local/share /usr/share)
set(GNC_DBD_DIR ${CMAKE_PREFIX_LIBDIR}/dbd CACHE PATH "specify location of libdbi drivers")
set(PKGLIBDIR ${CMAKE_INSTALL_LIBDIR}/gnucash)
set(TEST_MYSQL_URL "" CACHE STRING "MySQL database URL for testing")
set(TEST_PGSQL_URL "" CACHE STRING "PgSQL database URL for testing")

set(DATADIR_BUILD    ${CMAKE_BINARY_DIR}/${DATADIRNAME})
string(REPLACE ${CMAKE_INSTALL_PREFIX} "" LIBDIR_BUILD ${LIBDIR})
set(LIBDIR_BUILD     ${CMAKE_BINARY_DIR}/${LIBDIR_BUILD})
set(SYSCONFDIR_BUILD ${CMAKE_BINARY_DIR}/etc)
set(LIBEXECDIR_BUILD ${CMAKE_BINARY_DIR}/libexec)
set(BINDIR_BUILD     ${CMAKE_BINARY_DIR}/bin)

# We need to distinguish between MinGW.org and MinGW-w64:
if (MINGW)
  string(FIND ${CMAKE_C_COMPILER} "msys2" IS_MSYS2)
  if(${IS_MSYS2} GREATER_EQUAL 0)
    set(MINGW64 ON)
  endif()
endif()

# Find a proper bash executable

set(GNC_SHELL $ENV{GNC_SHELL})
if (GNC_SHELL) # Replacing this with if ($ENV{GNC_SHELL}) doesn't work.
  # Allow shell override by setting the GNC_SHELL environment variable
  set(SHELL ${GNC_SHELL})
elseif (MINGW AND NOT MINGW64)
  # Old mingw's bash is on on the path, so hard-code it for now
  set(SHELL ${CMAKE_PREFIX_PATH}/mingw/msys/1.0/bin/bash.exe)
else()
  find_package(UnixCommands)
  if (BASH)
    set(SHELL ${BASH})
  else()
    message(SEND_ERROR "Can't find a suitable bash executable. Please set GNC_SHELL.")
  endif()
endif()

# Determine whether we are building from a VCS or from a tarball
execute_process(
  COMMAND ${SHELL} ${CMAKE_SOURCE_DIR}/util/gnc-vcs-info -t ${CMAKE_SOURCE_DIR}
  RESULT_VARIABLE GNC_VCS_INFO_RESULT
  OUTPUT_VARIABLE GNC_VCS_INFO_OUTPUT
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

set(BUILDING_FROM_VCS "NO")
if (GNC_VCS_INFO_RESULT EQUAL 0)
  set(BUILDING_FROM_VCS "YES")
  # if building from VCS, we need git
  find_package(Git)
  if (NOT GIT_FOUND)
    message(SEND_ERROR "Looks like we're building from version control, but can't find git executable. Please set GIT_EXECUTABLE.")
  endif()
  set(VCS_INFO_BASE_DIR ${CMAKE_BINARY_DIR})
else()
  set(VCS_INFO_BASE_DIR ${CMAKE_SOURCE_DIR})
endif()
# The location of gnc-vcs-info.h depends on whether we build from VCS or not
# And this file is needed in several other build rules, so we define its
# location once here.
set(VCS_INFO_FILE ${VCS_INFO_BASE_DIR}/libgnucash/core-utils/gnc-vcs-info.h)

# Determine whether to generate the swig wrappers.
# By default they will only be generated when building from a git worktree
set(GENERATE_SWIG_WRAPPERS ${BUILDING_FROM_VCS} CACHE BOOL "Controls whether to generate the swig wrappers for guile and python. If not set the wrappers will only be generated when building from a git worktree")

if (WIN32)
  # Help Windows find the various dependencies. We assume here that the standard advice for building
  # GnuCash has been followed and that these dependencies live underneath something like C:/GCDEV, which
  # should be provided as CMAKE_PREFIX_PATH on the CMake command line:
  #      cmake -D CMAKE_PREFIX_PATH=c/gcdev -G "MSYS Makefiles" path/to/gnucash/sources
  #
  set(CMAKE_C_IMPLICIT_LINK_LIBRARIES "")
  set(CMAKE_FRAMEWORK_PATH_TMP ${CMAKE_PREFIX_PATH})
  set(DEV_SUBDIRS aqbanking gnome guile gwenhywfar libgsf libofx libsoup libxslt webkit)
  foreach(subdir ${DEV_SUBDIRS})
    list(APPEND CMAKE_FRAMEWORK_PATH_TMP ${CMAKE_PREFIX_PATH}/${subdir})
  endforeach()
  # Setting CMAKE_FRAMEWORK_PATH, which is intended for OS X, is a terrible hack here. But I don't
  # want to mess with CMAKE_PREFIX_PATH.
  set(CMAKE_FRAMEWORK_PATH "${CMAKE_FRAMEWORK_PATH_TMP}")
  #set(PKG_CONFIG_EXECUTABLE ${CMAKE_PREFIX_PATH}/gnome/bin/pkg-config.exe)
  set(SWIG_EXECUTABLE swig)
  #set(SWIG_EXECUTABLE ${CMAKE_PREFIX_PATH}/swig/swig.exe)
  find_path(REGEX_INCLUDE_PATH regex.h)
  find_path(REGEX_LIB_PATH libregex.dll.a)
  set(REGEX_LDFLAGS "-L${REGEX_LIB_PATH} -lregex")
  #set(LIBXSLT_INCLUDE_DIR ${CMAKE_PREFIX_PATH}/libxslt/include)
  #set(LIBXSLT_XSLTPROC_EXECUTABLE ${CMAKE_PREFIX_PATH}/libxslt/bin/xsltproc)
#Prevent creating a console window on startup.
  set(CMAKE_EXE_LINKER_FLAGS -mwindows)
endif()

find_package(PkgConfig REQUIRED)

if (NOT PKG_CONFIG_FOUND)
   message (SEND_ERROR "pkg-config not found, but is required")
 endif()

if (CMAKE_VERSION VERSION_LESS 3.6)
  include(GncPkgConfig)
endif()

# glib et al.
pkg_check_modules (GLIB2 REQUIRED glib-2.0>=2.40)
pkg_check_modules (GIO REQUIRED gio-2.0)
pkg_check_modules (GOBJECT REQUIRED gobject-2.0>=2.20)
pkg_check_modules (GMODULE REQUIRED gmodule-2.0>=2.20)
pkg_check_modules (GTHREAD REQUIRED gthread-2.0>=2.20)

pkg_check_modules (LIBXML2 REQUIRED libxml-2.0>=2.7.0)
pkg_check_modules (LIBXSLT REQUIRED libxslt)
if (WITH_GNUCASH)
  if (WIN32 OR APPLE)
    if (NOT CMAKE_VERSION VERSION_LESS 3.6)
      pkg_check_modules (WEBKIT REQUIRED IMPORTED_TARGET webkitgtk-3.0)
    else()
      pkg_check_modules (WEBKIT REQUIRED webkitgtk-3.0)
      _pkg_create_imp_target(WEBKIT)
    endif()
    set(WEBKIT1 1 CACHE INTERNAL "WebKitGtk")
  else()
    If (NOT CMAKE_VERSION VERSION_LESS 3.6)
      pkg_check_modules (WEBKIT IMPORTED_TARGET webkit2gtk-4.0)
    else()
      pkg_check_modules (WEBKIT webkit2gtk-4.0)
    endif()
    if (NOT WEBKIT_FOUND)
      if (NOT CMAKE_VERSION VERSION_LESS 3.6)
        pkg_check_modules (WEBKIT REQUIRED IMPORTED_TARGET webkit2gtk-3.0)
      else()
        pkg_check_modules (WEBKIT REQUIRED webkit2gtk-3.0)
      endif()
      set(WEBKIT2_3 1 CACHE INTERNAL "WebKit2Gtk3")
    else()
      if (NOT WEBKIT2_3)
        set(WEBKIT2_4 1 CACHE INTERNAL "WebKit2Gtk4")
      endif()
    endif()
    if (CMAKE_VERSION VERSION_LESS 3.6)
      _pkg_create_imp_target(WEBKIT)
    endif()
  endif()

  if (NOT CMAKE_VERSION VERSION_LESS 3.6)
    pkg_check_modules (GTK3 REQUIRED IMPORTED_TARGET gtk+-3.0>=3.10.0)
  else()
    pkg_check_modules (GTK3 REQUIRED gtk+-3.0>=3.10.0)
    _pkg_create_imp_target(GTK3)
  endif()
endif()

pkg_check_modules (ZLIB REQUIRED zlib)

if (MSVC)
  message (STATUS "Hint: To create the import libraries for the gnome DLLs (e.g. gconf-2.lib), use the dlltool as follows: pexports bin/libgconf-2-4.dll > lib/libgconf-2.def ; dlltool -d lib/libgconf-2.def -D bin/libgconf-2-4.dll -l lib/gconf-2.lib")

  # Disable the obnoxious min/max macros in MSVC - we want to use the
  # function versions of them.
  add_definitions ( -DNOMINMAX )
endif()

find_path (LTDL_INCLUDE_PATH NAMES ltdl.h PATHS /usr/include)
if(NOT LTDL_INCLUDE_PATH-NOTFOUND)
  set(HAVE_LTDL_H 1)
endif()

find_program(GLIB_COMPILE_SCHEMAS glib-compile-schemas HINTS ${CMAKE_PREFIX_PATH}/gnome/bin)
if (NOT GLIB_COMPILE_SCHEMAS)
  message(SEND_ERROR "Can't find glib-compile-schemas program. Please set GLIB_COMPILE_SCHEMAS.")
endif()

find_path (REGEX_INCLUDE_PATH NAMES regex.h
		  PATHS /usr/include /opt/gnome/include)
find_library (REGEX_LIBRARY NAMES regex)

# I18N
# Potfile generation will only be enabled if building from a git worktree
set (BUILD_GNUCASH_POT ${BUILDING_FROM_VCS})
set (TRANSLATE_GNUCASH_APPDATA ON)
set (TRANSLATE_GNUCASH_DESKTOP ON)

find_package (Gettext 0.18.1 REQUIRED)
if (${GETTEXT_VERSION_STRING} VERSION_LESS 0.20)
    message (WARNING "Gettext version 0.20 or more recent is required to translate the 'developer_name' tag in gnucash.appdata.xml. All but that tag will be translated in the generated file.")
    if(BUILD_GNUCASH_POT)
        # Only emit warning if potfile generation was enabled otherwise
        message (WARNING "Gettext version 0.20 or more recent is required to extract all translatable strings. Potfile generation will be disabled.")
    endif()
    set (BUILD_GNUCASH_POT OFF)
endif()
if (${GETTEXT_VERSION_STRING} VERSION_LESS 0.19.6)
    set (TRANSLATE_GNUCASH_APPDATA OFF)
    message (WARNING "Gettext version 0.19.6 or more recent is required to translate gnucash.appdata.xml. The build will be configured to generate an untranslated gnucash.appdata.xml.")
endif()
if (${GETTEXT_VERSION_STRING} VERSION_LESS 0.19)
    set (TRANSLATE_GNUCASH_DESKTOP OFF)
    message (WARNING "Gettext version 0.19 or more recent is required to translate gnucash.desktop. The build will be configured to generate an untranslated gnucash.desktop.")
endif()

find_path (LIBINTL_INCLUDE_PATH NAMES libintl.h
		  PATHS /usr/include /opt/gnome/include)
find_library (LIBINTL_LIBRARY NAMES intl)

# HELP
if (WIN32)
  message(STATUS "Looking for htmlhelp.h and htmlhelp.a")
  find_path (HTMLHELP_INCLUDE_PATH NAMES htmlhelp.h)
  find_library (HTMLHELP_LIBRARY htmlhelp)
endif()

# ############################################################

# SWIG
if(GENERATE_SWIG_WRAPPERS)
  find_package (SWIG 2.0.10 REQUIRED)
  include (${SWIG_USE_FILE})
  string(REGEX MATCH "^[0-9]+[.]" SWIG_MAJOR ${SWIG_VERSION})
endif()

# Find Guile and determine which version we are using.
# Look for guile versions in this order: 2.2 > 2.0

# guile library and include dir
pkg_check_modules (GUILE22 guile-2.2 QUIET)
if (GUILE22_FOUND) # found guile-2.2
  add_definitions (-DHAVE_GUILE22)
  set(HAVE_GUILE2 TRUE)
  set(GUILE_EFFECTIVE_VERSION 2.2)
  set(GUILE_INCLUDE_DIRS ${GUILE22_INCLUDE_DIRS})
  set(GUILE_LDFLAGS ${GUILE22_LDFLAGS})

  find_program (GUILD_EXECUTABLE NAMES guild2.2 guild)
  if (NOT GUILD_EXECUTABLE)
    message (SEND_ERROR "The guild executable was not found, but is required. Please set GUILD_EXECUTABLE.")
  endif()
  message(STATUS "Using guile-2.2.x")
  find_program (GUILE_EXECUTABLE NAMES guile2.2 guile)
else()
  pkg_check_modules (GUILE2 guile-2.0>=2.0.9 QUIET)
  if (GUILE2_FOUND) # found guile-2.0
    add_definitions (-DHAVE_GUILE20)
    set(HAVE_GUILE2 TRUE)
    set(GUILE_EFFECTIVE_VERSION 2.0)
    set(GUILE_INCLUDE_DIRS ${GUILE2_INCLUDE_DIRS})
    set(GUILE_LDFLAGS ${GUILE2_LDFLAGS})

    find_program (GUILD_EXECUTABLE NAMES guild2.0 guild)
    if (NOT GUILD_EXECUTABLE)
      message (SEND_ERROR "The guild executable was not found, but is required. Please set GUILD_EXECUTABLE.")
    endif()
    message(STATUS "Using guile-2.0.x")
    find_program (GUILE_EXECUTABLE NAMES guile2.0 guile)
  else()
    message (FATAL_ERROR "Neither guile 2.2 nor guile 2.0 were found GnuCash can't run without one of them. Ensure that one is installed and can be found with pkg-config.")
  endif()
endif()

if (NOT GUILE_EXECUTABLE)
  message (SEND_ERROR "The guile executable was not found, but is required. Please set GUILE_EXECUTABLE.")
endif()

# Test that guile has SRFI-64. This is required for some unit tests.
execute_process (COMMAND ${GUILE_EXECUTABLE} -c "(use-modules (srfi srfi-64))"
  RESULT_VARIABLE GNC_SRFI64_RESULT
  ERROR_QUIET
)

if (GNC_SRFI64_RESULT EQUAL 0)
  message (STATUS "Using guile SRFI-64")
  set (HAVE_SRFI64 TRUE)
endif()

# Test that guile has textual-ports. This is required for the stress test.
execute_process (COMMAND ${GUILE_EXECUTABLE} -c "(use-modules (ice-9 textual-ports))"
  RESULT_VARIABLE GNC_TEXT_PORTS_RESULT
  ERROR_QUIET
)

if (GNC_TEXT_PORTS_RESULT EQUAL 0)
  message (STATUS "Using guile textual-ports")
  set (HAVE_TEXT_PORTS TRUE)
endif()

# ############################################################
if (WITH_AQBANKING)
  pkg_check_modules (GWENHYWFAR REQUIRED gwenhywfar>=4.9.99)
  pkg_check_modules (AQBANKING REQUIRED aqbanking>=5.3.4)
  if(WITH_GNUCASH)
    pkg_check_modules (GWEN_GTK3 gwengui-gtk3)
    if(GWEN_GTK3_FOUND AND GWEN_GTK3_VERSION VERSION_GREATER "4.20.0")
      set(HAVE_GWEN_GTK3 1 CACHE BOOL "True if gwen-gtk3.pc exists")
    endif()
  endif()
endif()

if (WITH_OFX)
  pkg_check_modules (LIBOFX REQUIRED libofx)
  include(CheckCXXSourceRuns)
  if (WIN32)
      set(CMAKE_REQUIRED_LIBRARIES "-L ${CMAKE_PREFIX_PATH}/libofx/lib -lofx")
  else()
      set(CMAKE_REQUIRED_LIBRARIES "-lofx")
  endif()
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
  set(HAVE_OFX_BUG_39 ${HAVE_OFX_BUG_39})
  set(CMAKE_REQUIRED_LIBRARIES)
endif()
# ############################################################

if(APPLE)
  execute_process(
      COMMAND ${PKG_CONFIG_EXECUTABLE} --variable=targets gdk-3.0
      OUTPUT_VARIABLE TARGET_RESULT
      OUTPUT_STRIP_TRAILING_WHITESPACE
  )
  if(TARGET_RESULT STREQUAL "quartz")
    set(GNC_PLATFORM_COCOA 1)
    set(GDK_QUARTZ 1)
    pkg_check_modules(GTK_MAC gtk-mac-integration-gtk3)
    if (GTK_MAC_FOUND)
      set(MAC_INTEGRATION 1)
      find_library(COCOA_LIBRARY Cocoa)
      find_library(SECURITY_LIBRARY Security)
      find_library(CARBON_LIBRARY Carbon)
      set(OSX_EXTRA_COMPILE_FLAGS $<$<COMPILE_LANGUAGE:C>:-xobjective-c> $<$<COMPILE_LANGUAGE:CXX>:-xobjective-c++>)
      set(OSX_EXTRA_LIBRARIES objc ${COCOA_LIBRARY} ${SECURITY_LIBRARY} ${CARBON_LIBRARY})
    endif()
  endif()
endif()

# find_package(LibXslt) eats PKG_CONFIG_EXECUTABLE, so preserve it.
set(GNC_PKG_CONFIG_EXE ${PKG_CONFIG_EXECUTABLE})
# ############################################################

# xsltproc
find_package(LibXslt)

if (NOT LIBXSLT_FOUND)
   message(FATAL_ERROR "libxslt library not found.")
endif()
if (${LIBXSLT_XSLTPROC_EXECUTABLE} STREQUAL "LIBXSLT_XSLTPROC_EXECUTABLE-NOTFOUND")
  message(FATAL_ERROR "xsltproc executable not found")
endif()
# ############################################################
set(PKG_CONFIG_EXECUTABLE ${GNC_PKG_CONFIG_EXE})

# libdbi
find_path (LIBDBI_INCLUDE_PATH dbi/dbi.h)
find_library (LIBDBI_LIBRARY dbi)
find_library (LIBDBI_DRIVERS_DIR
  NAMES dbdmysql dbdpgsql dbdsqlite3 NAMES_PER_DIR
  PATH_SUFFIXES dbd libdbi-drivers/dbd
  HINTS LIBDBI_LIBRARY
  PATHS GNC_DBD_DIR
  DOC "Libdbi Drivers Directory")
if (WITH_SQL)
  if (NOT LIBDBI_INCLUDE_PATH)
    message (SEND_ERROR "Include file <dbi/dbi.h> was not found - did you install libdbi0-dev or libdbi-dev?")
  endif()
  if (NOT LIBDBI_LIBRARY)
    message (SEND_ERROR "Library libdbi was not found")
  endif()
  set(HAVE_DBI_DBI_H 1)
  if (NOT LIBDBI_DRIVERS_DIR)
    message (SEND_ERROR "No libdbi drivers found, SQL tests will fail.")
  else()
    get_filename_component(drivers_dir ${LIBDBI_DRIVERS_DIR} DIRECTORY)
    set(LIBDBI_DRIVERS_DIR ${drivers_dir} CACHE FILEPATH "Directory containing the libdbi driver modules." FORCE)
  endif()
endif()

# ############################################################

if (WITH_PYTHON)
  find_package(PythonInterp 3)
  if (NOT PYTHONINTERP_FOUND)
    message(SEND_ERROR "Python support enabled, but Python interpreter not found.")
  endif()

  if (PYTHON_VERSION_STRING VERSION_LESS "3.2.0")
    message(SEND_ERROR "Found python version ${PYTHON_VERSION_STRING}, but it's too old. Need python >= 3.2.0")
  endif()

  find_package(PythonLibs 3)
  if (NOT PYTHONLIBS_FOUND)
    message(SEND_ERROR "Python support enabled, but Python libraries not found.")
  endif()

  # Determine where to install the python libraries.
  execute_process(
    COMMAND ${PYTHON_EXECUTABLE} -c "from distutils import sysconfig; print(sysconfig.get_python_lib(prefix='${CMAKE_INSTALL_PREFIX}', plat_specific=True))"
    RESULT_VARIABLE PYTHON_SYSCONFIG_RESULT
    OUTPUT_VARIABLE PYTHON_SYSCONFIG_OUTPUT
    ERROR_VARIABLE PYTHON_SYSCONFIG_ERROR
    OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_STRIP_TRAILING_WHITESPACE
  )
  if (PYTHON_SYSCONFIG_RESULT)
    message(SEND_ERROR "Could not determine Python site-package directory:\n${PYTHON_SYSCONFIG_ERROR}")
  endif()
  string(REPLACE ${CMAKE_INSTALL_PREFIX} ${CMAKE_BINARY_DIR} PYTHON_SYSCONFIG_BUILD ${PYTHON_SYSCONFIG_OUTPUT})
endif()



find_package(Perl)

if (NOT PERL_FOUND)
  message(SEND_ERROR "Perl executable not found. Please set PERL_EXECUTABLE.")
endif()

get_filename_component(PERL_DIR ${PERL_EXECUTABLE} DIRECTORY)

find_program(POD2MAN_EXECUTABLE pod2man HINTS ${PERL_DIR})

#ICU
pkg_check_modules (ICU4C REQUIRED icu-uc)
pkg_check_modules (ICU4C_I18N REQUIRED icu-i18n)

pkg_check_modules (LIBSECRET libsecret-1>=0.18)
IF (LIBSECRET_FOUND)
  SET (HAVE_LIBSECRET ON)
ENDIF (LIBSECRET_FOUND)

#BOOST
set (Boost_USE_MULTITHREADED ON)
set (Boost_FIND_QUIETLY ON)

if (NOT DEFINED ${BOOST_ROOT})
  set(BOOST_ROOT $ENV{BOOST_ROOT})
endif()
find_package (Boost 1.54.0 REQUIRED COMPONENTS date_time regex locale filesystem system)

if (Boost_FOUND)
  include_directories(${Boost_INCLUDE_DIRS})
  set(HAVE_BOOST 1)
else()
  message (SEND_ERROR "Boost 1.54.0 or later is not installed, and is required. Please install it and ensure that the following libraries are built: chrono, date_time, filesystem, log, program_options, regex, signals, system, and test.")
endif()


# Compiler flags

include (CheckCCompilerFlag)
include (CheckCXXCompilerFlag)
check_c_compiler_flag(-Wstringop-truncation have_stringop_truncation)
if (have_stringop_truncation)
    set(HAVE_STRINGOP_TRUNCATION TRUE)
endif()

if (SWIG_MAJOR LESS 3)
check_cxx_compiler_flag(-Wno-register have_no_register)
if (have_no_register)
  set(REGISTER_CXXFLAG -Wno-register)
else()
  check_cxx_compiler_flag(-Wno-deprecated-register have_no_dep_register)
  if (have_no_dep_register)
    set(REGISTER_CXXFLAG -Wno-deprecated-register)
  endif()
endif()
endif()
add_definitions(-D_GNU_SOURCE)

# Also, set the C++ version to c++11
set(CMAKE_CXX_FLAGS "-std=gnu++11 ${CMAKE_CXX_FLAGS}")  # FIXME: should be -std=c++11


if (UNIX)
  set( CMAKE_C_FLAGS "-Werror -Wdeclaration-after-statement -Wno-pointer-sign -Wall -Wmissing-prototypes -Wmissing-declarations -Wno-unused ${CMAKE_C_FLAGS}")
  set( CMAKE_C_FLAGS "-Wno-error=deprecated-declarations -std=gnu11 -Wno-error=parentheses ${CMAKE_C_FLAGS}")
  set( CMAKE_CXX_FLAGS "-Werror -Wall -Wmissing-declarations -Wno-unused -Wno-error=parentheses ${CMAKE_CXX_FLAGS}")
  set( CMAKE_CXX_FLAGS "-Wno-error=deprecated-declarations ${REGISTER_CXXFLAG} ${CMAKE_CXX_FLAGS}")
  set( CMAKE_C_FLAGS_RELEASE "-O3 -U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=2 ${CMAKE_C_FLAGS}")
endif()
if (MINGW)
  set( CMAKE_C_FLAGS "-Werror -Wdeclaration-after-statement -Wno-pointer-sign -Wall -Wunused -Wmissing-prototypes -Wmissing-declarations  -Wno-unused -Wno-error=deprecated-declarations -std=gnu11 ${CMAKE_C_FLAGS}")
  set( CMAKE_CXX_FLAGS "-DWINVER=0x0500 -D_EMULATE_GLIBC=0 ${CMAKE_CXX_FLAGS}") # Workaround for bug in gtest on mingw, see https://github.com/google/googletest/issues/893 and https://github.com/google/googletest/issues/920
endif()

if (APPLE AND WITH_GNUCASH)
  set(CMAKE_MACOSX_RPATH ON)
  set(CMAKE_INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}")
endif()

if (UNIX)
  set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_FULL_LIBDIR}:${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
endif()

set(BUILD_SHARED_LIBS ON)

# Most libraries are installed to lib/gnucash, so set that as the default.
# For the handful that are installed to lib, we override the properties below
# (after the targets have been read in).


set(CMAKE_LIBRARY_OUTPUT_DIRECTORY         ${LIBDIR_BUILD}/gnucash)
if (XCODE_VERSION)
  # FIXME: These settings break the ability to manipulate different configurations (Debug,
  # FIXME: Release, etc.) in Xcode. We'll need to change the module loading C code if
  # we want to support multi config.
  set(CMAKE_LIBRARY_OUTPUT_DIRECTORY_DEBUG   ${LIBDIR_BUILD}/gnucash)
  set(CMAKE_LIBRARY_OUTPUT_DIRECTORY_RELEASE ${LIBDIR_BUILD}/gnucash)
endif()

# For binarines
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY         ${CMAKE_BINARY_DIR}/bin)
if (XCODE_VERSION)
  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_DEBUG   ${CMAKE_BINARY_DIR}/bin)
  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_RELEASE ${CMAKE_BINARY_DIR}/bin)
endif()
# ############################################################

gnc_gtest_configure()

# There are targets that need to build before tests will run
add_custom_target(check
  COMMAND ${CMAKE_CTEST_COMMAND}
)

set(gnucash_DOCS
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
    ChangeLog.2018
    ChangeLog.2019
    DOCUMENTERS
    HACKING
    LICENSE
    NEWS
    README.dependencies
)


install(FILES ${gnucash_DOCS} DESTINATION ${CMAKE_INSTALL_DOCDIR})

# ############################################################

# Create config.h

if (WIN32)
if (MINGW)
set (HAVE_SCANF_LLD 1)
else ()
set (HAVE_SCANF_I64D 1)
endif()
set (HAVE_HTMLHELPW 1)
endif()

check_include_files (dirent.h HAVE_DIRENT_H)
check_include_files (dlfcn.h HAVE_DLFCN_H)
check_include_files (glob.h HAVE_GLOB_H)
check_include_files (inttypes.h HAVE_INTTYPES_H)
check_include_files (limits.h HAVE_LIMITS_H)
check_include_files (locale.h HAVE_LOCALE_H)
check_include_files (memory.h HAVE_MEMORY_H)
check_include_files (stdint.h HAVE_STDINT_H)
check_include_files (stdlib.h HAVE_STDLIB_H)
check_include_files (string.h HAVE_STRING_H)
check_include_files (strings.h HAVE_STRINGS_H)
check_include_files (sys/stat.h HAVE_SYS_STAT_H)
check_include_files (sys/time.h HAVE_SYS_TIME_H)
check_include_files (sys/times.h HAVE_SYS_TIMES_H)
check_include_files (sys/types.h HAVE_SYS_TYPES_H)
check_include_files (sys/wait.h HAVE_SYS_WAIT_H)
check_include_files (unistd.h HAVE_UNISTD_H)
check_include_files (utmp.h HAVE_UTMP_H)
check_include_files (wctype.h HAVE_WCTYPE_H)

test_big_endian(IS_BIGENDIAN)
if (IS_BIGENDIAN)
  set(WORDS_BIGENDIAN)
endif()

if (NOT DISABLE_NLS)
set(ENABLE_NLS 1)
endif()

if (ENABLE_BINRELOC)
if (UNIX OR MINGW)
set(BR_PTHREAD 1)
endif()
endif()

if (UNIX OR MINGW)
set (HAVE_GETTIMEOFDAY 1)
set (HAVE_GUILE 1)
set (HAVE_LIBM 1)
set (HAVE_MEMCPY 1)
set (STDC_HEADERS 1)
set (_ALL_SOURCE 1)
set (_GNU_SOURCE 1)
set (_POSIX_PTHREAD_SEMANTICS 1)
set (_TANDEM_SOURCE 1)
set (__EXTENSIONS__ 1)
endif()

if (UNIX)
set (HAVE_CHOWN 1)
set (HAVE_DLERROR 1)
set (HAVE_DLSYM 1)
set (HAVE_GETHOSTID 1)
set (HAVE_GETHOSTNAME 1)
set (HAVE_GETPPID 1)
set (HAVE_GETUID 1)
set (HAVE_GMTIME_R 1)
set (HAVE_LANGINFO_D_FMT 1)
set (HAVE_LC_MESSAGES 1)
set (HAVE_LIBPTHREAD 1)
set (HAVE_LINK 1)
set (HAVE_LOCALTIME_R 1)
set (HAVE_PTHREAD_MUTEX_INIT 1)
set (HAVE_PTHREAD_PRIO_INHERIT 1)
set (HAVE_SCANF_LLD 1)
set (HAVE_SETENV 1)
set (HAVE_STPCPY 1)
set (HAVE_STRPTIME 1)
set (HAVE_STRUCT_TM_GMTOFF 1)
set (HAVE_TIMEGM 1)
set (HAVE_TOWUPPER 1)
set (GNC_PLATFORM_POSIX 1)
endif()

if (WIN32)
set (GNC_PLATFORM_WINDOWS 1)
endif()

if (APPLE)
# FIXME: HANDLE gtk-mac-integration-gtk2
set(GNC_PLATFORM_DARWIN 1)
set(GNC_PLATFORM_OSX 1)
set(PLATFORM_OSX 1)
set(HAVE_OSX_KEYCHAIN 1)
endif()

if(GLIB2_VERSION VERSION_GREATER 2.46.0 OR GLIB2_VERSION VERSION_EQUAL 2.46.0)
set(HAVE_GLIB_2_46 1)
endif()

if(DISABLE_DEPRECATED_GNOME)
set(GNOME_DISABLE_DEPRECATED 1)
endif()

if(DISABLE_DEPRECATED_GTK)
set(GTK_DISABLE_DEPRECATED 1)
set(GDK_DISABLE_DEPRECATED 1)
set(GDK_PIXMAP_DISABLE_DEPRECATED 1)
endif()

if(DISABLE_DEPRECATED_GLIB)
set(G_DISABLE_DEPRECATED 1)
endif()

add_definitions (-DHAVE_CONFIG_H)

set (CONFIG_H ${CMAKE_CURRENT_BINARY_DIR}/common/config.h)
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/common/config.h.cmake.in ${CONFIG_H})

set(SCHEME_INSTALLED_SOURCE_DIR ${CMAKE_INSTALL_DATADIR}/gnucash/scm)
set(SCHEME_INSTALLED_CACHE_DIR ${PKGLIBDIR}/scm/ccache/${GUILE_EFFECTIVE_VERSION})

# The subdirectories
add_subdirectory (borrowed)
add_subdirectory (data)
add_subdirectory (doc)
add_subdirectory (po)
add_subdirectory (common)
add_subdirectory (libgnucash)
if (WITH_GNUCASH)
  add_subdirectory (gnucash)
endif()
add_subdirectory (bindings)
add_subdirectory (test-templates)
add_subdirectory (util)

# This cmake subdir must be the last add_subdirectory() call because
# it contains post-install actions to execute.
add_subdirectory(cmake)

# Generate the ChangeLog

if (BUILDING_FROM_VCS)
    add_custom_target(ChangeLog ALL
        COMMAND ${GIT_EXECUTABLE} log --format=\"%ad %aN %n%n%x09* %s%d%n\" --date=short --since=2020-01-01 > ${CMAKE_BINARY_DIR}/ChangeLog
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    )
    install(FILES ${CMAKE_BINARY_DIR}/ChangeLog DESTINATION ${CMAKE_INSTALL_DOCDIR})
else()
    install(FILES ${CMAKE_SOURCE_DIR}/ChangeLog DESTINATION ${CMAKE_INSTALL_DOCDIR})
endif()
dist_add_generated (${BUILDING_FROM_VCS} ChangeLog)

############################ BEGIN MAKE DIST #################

set(PACKAGE_PREFIX "${PROJECT_NAME}-${PROJECT_VERSION}")
set(DIST_FILE "${PACKAGE_PREFIX}.tar")

set(toplvl_DIST_local ${gnucash_DOCS}
  CMakeLists.txt README)


set_local_dist(toplvl_DIST ${toplvl_DIST_local})

# Each subdirectory is responsible for reporting its list of distribution files to its parent, up to here.
#
# Leaf directories use the set_dist_list() function to report their distribution files. See
# the bottom of libgnucash/app-utils/test/CMakeLists.txt for an example.
#
# A non leaf directories uses the set_local_dist() function to specify its distribution files local that dir,
# then uses a (set ${foo_DIST} ${locals....} PARENT_SCOPE) command to report up. See the bottom of
# libgnucash/app-utils/CMakeLists.txt for an example of this.

set(ALL_DIST ${bindings_DIST} ${borrowed_DIST} ${cmake_DIST} ${common_DIST} ${data_DIST}
    ${doc_DIST} ${gnucash_DIST} ${libgnucash_DIST} ${packaging_DIST}
    ${po_DIST} ${test_templates_DIST} ${toplvl_DIST} ${util_DIST})


if (BUILDING_FROM_VCS)
  set(BUILD_SOURCE_DIR ${CMAKE_BINARY_DIR})
else()
  set(BUILD_SOURCE_DIR ${CMAKE_SOURCE_DIR})
endif()

# Write a dist manifest
string(REPLACE ";" "\n" ALL_DIST_LINES "${ALL_DIST}")
file(WRITE ${CMAKE_BINARY_DIR}/dist_manifest.txt ${ALL_DIST_LINES})

add_custom_command(OUTPUT ${DIST_FILE}.gz ${DIST_FILE}.bz2
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

add_custom_target(dist DEPENDS ${DIST_FILE}.gz ${DIST_FILE}.bz2)

add_custom_target(distcheck DEPENDS dist
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

set(_MODULES gnc-core-utils gnc-module gnc-backend-xml-utils)

if (WITH_SQL)
  list(APPEND _MODULES gnc-backend-sql)
endif()

if (WITH_GNUCASH)
  list(APPEND _MODULES gnc-gnome)
endif()

set_target_properties(${_MODULES} PROPERTIES
  LIBRARY_OUTPUT_DIRECTORY         ${LIBDIR_BUILD}
  if (XCODE_VERSION)
    LIBRARY_OUTPUT_DIRECTORY_DEBUG   ${LIBDIR_BUILD}
    LIBRARY_OUTPUT_DIRECTORY_RELEASE ${LIBDIR_BUILD}
  endif()

  RUNTIME_OUTPUT_DIRECTORY         ${CMAKE_BINARY_DIR}/bin
  if (XCODE_VERSION)
    RUNTIME_OUTPUT_DIRECTORY_DEBUG   ${CMAKE_BINARY_DIR}/bin
    RUNTIME_OUTPUT_DIRECTORY_RELEASE ${CMAKE_BINARY_DIR}/bin
  endif()
)

if (WIN32)
  set_target_properties(gncmod-backend-xml gncmod-stylesheets PROPERTIES
    LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
  )
endif()

#Link LICENSE to COPYING so that people expecting to find it,
#will. COPYING is normally linked by autogen.sh to the latest
#automake-provided version.
set (_CMD create_symlink)
if (WIN32)
  set(_CMD copy)
endif()

install(CODE
" execute_process(
      COMMAND ${CMAKE_COMMAND} -E ${_CMD} LICENSE COPYING
      WORKING_DIRECTORY ${CMAKE_INSTALL_DOCDIR}
  )"
)

#For windows, copy in some DLLs from Mingw

if (WIN32)
  find_library(LIBSTDC++ libstdc++-6.dll)
  find_library(LIBDW2 libgcc_s_dw2-1.dll)
  set(MINGW_DLLS ${LIBSTDC++} ${LIBDW2})
  install(PROGRAMS ${MINGW_DLLS} DESTINATION ${CMAKE_INSTALL_BINDIR})
  file(COPY ${MINGW_DLLS} DESTINATION ${BINDIR_BUILD}
    # Do permissions matter for windows?
    FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
  )
endif()
# ############################################################
# Package creation rules

if(UNIX)
  set(CPACK_GENERATOR "TGZ")
endif()

include (CPack)
