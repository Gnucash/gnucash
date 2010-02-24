# CMakeLists.txt for src/

# EXPERIMENTAL! This is just a trial of how far we can get for a cmake
# build system.

CMAKE_MINIMUM_REQUIRED (VERSION 2.6)
PROJECT (gnucash)

# Find pkg-config
FIND_PACKAGE (PkgConfig REQUIRED)

# glib
PKG_CHECK_MODULES (GLIB2 glib-2.0>=2.12.0)

IF (NOT GLIB2_FOUND)
   MESSAGE (SEND_ERROR "Gtk not found, but is required")
ENDIF (NOT GLIB2_FOUND)

FIND_PATH (LIBINTL_INCLUDE_PATH NAMES libintl.h
		  PATHS /usr/include /opt/gnome/include)
FIND_PATH (REGEX_INCLUDE_PATH NAMES regex.h
		  PATHS /usr/include /opt/gnome/include)

#SET (QT_MIN_VERSION "4.5.0") # We need at least 4.5.0 (because only this is LGPL)
#FIND_PACKAGE (Qt4 REQUIRED) # find and setup Qt4 for this project
#INCLUDE_DIRECTORIES (${QT_INCLUDES})

ADD_DEFINITIONS (-DHAVE_CONFIG_H)

IF (UNIX)
  SET( CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Werror -Wdeclaration-after-statement -Wno-pointer-sign -D_FORTIFY_SOURCE=2 -Wall -Wunused -Wmissing-prototypes -Wmissing-declarations  -Wno-unused")
ENDIF (UNIX)

# Create config.h
SET (CONFIG_H ${CMAKE_CURRENT_BINARY_DIR}/config.h)
FILE (WRITE ${CONFIG_H} "#define PACKAGE \"gnucash\"\n")
IF (WIN32)
  FILE (APPEND ${CONFIG_H} "#define HAVE_SCANF_I64D 1\n")
ENDIF (WIN32)
IF (UNIX)
  FILE (APPEND ${CONFIG_H} "#define HAVE_SCANF_LLD 1\n")
ENDIF (UNIX)
IF (UNIX OR MINGW)
  FILE (APPEND ${CONFIG_H} "#define HAVE_SYS_TIME_H 1\n")
  FILE (APPEND ${CONFIG_H} "#define HAVE_UNISTD_H 1\n")
  FILE (APPEND ${CONFIG_H} "#define HAVE_DIRENT_H 1\n")
ENDIF (UNIX OR MINGW)

IF (UNIX)
  FILE (APPEND ${CONFIG_H} "
#define HAVE_BIND_TEXTDOMAIN_CODESET 1
#define HAVE_CHOWN 1
#define HAVE_DCGETTEXT 1
#define HAVE_DLERROR 1
#define HAVE_DLFCN_H 1
#define HAVE_GETHOSTID 1
#define HAVE_GETHOSTNAME 1
#define HAVE_GETPPID 1
#define HAVE_GETTEXT 1
#define HAVE_GETTIMEOFDAY 1
#define HAVE_GETUID 1
#define HAVE_GMTIME_R 1
#define HAVE_INTTYPES_H 1
#define HAVE_LANGINFO_D_FMT 1
#define HAVE_LC_MESSAGES 1
#define HAVE_LIBM 1
#define HAVE_LIBPTHREAD 1
#define HAVE_LIBQOF /**/
#define HAVE_LIMITS_H 1
#define HAVE_LINK 1
#define HAVE_LOCALE_H 1
#define HAVE_LOCALTIME_R 1
#define HAVE_MEMCPY 1
#define HAVE_MEMORY_H 1
#define HAVE_PTHREAD_MUTEX_INIT 1
#define HAVE_SETENV 1
#define HAVE_STDINT_H 1
#define HAVE_STDLIB_H 1
#define HAVE_STPCPY 1
#define HAVE_STRINGS_H 1
#define HAVE_STRING_H 1
#define HAVE_STRPTIME 1
#define HAVE_STRUCT_TM_GMTOFF 1
#define HAVE_SYS_STAT_H 1
#define HAVE_SYS_TIMES_H 1
#define HAVE_SYS_TIME_H 1
#define HAVE_SYS_TYPES_H 1
#define HAVE_SYS_WAIT_H 1
#define HAVE_TIMEGM 1
#define HAVE_TOWUPPER 1
#define HAVE_UNISTD_H 1
#define HAVE_UTMP_H 1
#define HAVE_WCTYPE_H 1
#define HAVE_X11_XLIB_H 1

/* Don't use deprecated qof functions */
#define QOF_DISABLE_DEPRECATED 1
/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Enable extensions on AIX 3, Interix.  */
#ifndef _ALL_SOURCE
# define _ALL_SOURCE 1
#endif
/* Enable GNU extensions on systems that have them.  */
#ifndef _GNU_SOURCE
# define _GNU_SOURCE 1
#endif
/* Enable threading extensions on Solaris.  */
#ifndef _POSIX_PTHREAD_SEMANTICS
# define _POSIX_PTHREAD_SEMANTICS 1
#endif
/* Enable extensions on HP NonStop.  */
#ifndef _TANDEM_SOURCE
# define _TANDEM_SOURCE 1
#endif
/* Enable general extensions on Solaris.  */
#ifndef __EXTENSIONS__
# define __EXTENSIONS__ 1
#endif

/* Version number of package */
#define VERSION "2.3.10"

/* Define WORDS_BIGENDIAN to 1 if your processor stores words with the most
   significant byte first (like Motorola and SPARC, unlike Intel). */
#if defined AC_APPLE_UNIVERSAL_BUILD
# if defined __BIG_ENDIAN__
#  define WORDS_BIGENDIAN 1
# endif
#else
# ifndef WORDS_BIGENDIAN
/* #  undef WORDS_BIGENDIAN */
# endif
#endif
")
ENDIF (UNIX)

# ############################################################

# The subdirectories
ADD_SUBDIRECTORY (libqof)

