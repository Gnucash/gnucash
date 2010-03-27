# CMakeLists.txt for src/

# ############################################################

# Create config.h
ADD_DEFINITIONS (-DHAVE_CONFIG_H)

SET (CONFIG_H ${CMAKE_CURRENT_BINARY_DIR}/config.h)
FILE (WRITE ${CONFIG_H} "#define PACKAGE \"gnucash\"\n")

FILE (APPEND ${CONFIG_H} "
#define GNUCASH_MAJOR_VERSION 2
#define GNUCASH_MICRO_VERSION 10
#define GNUCASH_MINOR_VERSION 3
#define VERSION "2.3.10"

/* Definitions for all OS */
#define HAVE_LIBQOF /**/
#define HAVE_TOWUPPER 1
#define QOF_DISABLE_DEPRECATED 1
#define GNC_NO_LOADABLE_MODULES 1
")

IF (WIN32)
  FILE (APPEND ${CONFIG_H} "
#define HAVE_SCANF_I64D 1
#define HAVE_HTMLHELPW 1
#define OS_WIN32 1
")
ENDIF (WIN32)

IF (MINGW)
  FILE (APPEND ${CONFIG_H} "
#define HAVE_PUTENV 1
")
ENDIF (MINGW)

IF (UNIX OR MINGW)
  FILE (APPEND ${CONFIG_H} "
#define GETTEXT_PACKAGE \"gnucash\"
#define HAVE_BIND_TEXTDOMAIN_CODESET 1
#define HAVE_DIRENT_H 1
#define HAVE_DCGETTEXT 1
#define HAVE_SYS_TIME_H 1
#define HAVE_UNISTD_H 1
#define HAVE_GETTEXT 1
#define HAVE_GETTIMEOFDAY 1
#define HAVE_GUILE 1
#define HAVE_INTTYPES_H 1
#define HAVE_LIBM 1
#define HAVE_LIMITS_H 1
#define HAVE_LOCALE_H 1
#define HAVE_MEMCPY 1
#define HAVE_MEMORY_H 1
#define HAVE_STDINT_H 1
#define HAVE_STDLIB_H 1
#define HAVE_STRINGS_H 1
#define HAVE_STRING_H 1
#define HAVE_SYS_STAT_H 1
#define HAVE_SYS_TIME_H 1
#define HAVE_SYS_TYPES_H 1
#define HAVE_UNISTD_H 1
#define HAVE_WCTYPE_H 1

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
ENDIF (UNIX OR MINGW)

IF (UNIX)
  FILE (APPEND ${CONFIG_H} "
#define HAVE_CHOWN 1
#define HAVE_DLERROR 1
#define HAVE_DLFCN_H 1
#define HAVE_GETHOSTID 1
#define HAVE_GETHOSTNAME 1
#define HAVE_GETPPID 1
#define HAVE_GETUID 1
#define HAVE_GMTIME_R 1
#define HAVE_LANGINFO_D_FMT 1
#define HAVE_LC_MESSAGES 1
#define HAVE_LIBPTHREAD 1
#define HAVE_LINK 1
#define HAVE_LOCALTIME_R 1
#define HAVE_PTHREAD_MUTEX_INIT 1
#define HAVE_SCANF_LLD 1
#define HAVE_SETENV 1
#define HAVE_STPCPY 1
#define HAVE_STRPTIME 1
#define HAVE_STRUCT_TM_GMTOFF 1
#define HAVE_SYS_TIMES_H 1
#define HAVE_SYS_WAIT_H 1
#define HAVE_TIMEGM 1
#define HAVE_UTMP_H 1
#define HAVE_X11_XLIB_H 1
")
ENDIF (UNIX)

# ############################################################

# The subdirectories
ADD_SUBDIRECTORY (libqof)
ADD_SUBDIRECTORY (core-utils)
ADD_SUBDIRECTORY (gnc-module)
ADD_SUBDIRECTORY (engine)
ADD_SUBDIRECTORY (backend/xml)
ADD_SUBDIRECTORY (business/business-core)
ADD_SUBDIRECTORY (calculation)
ADD_SUBDIRECTORY (app-utils)
IF (WITH_SQL)
  ADD_SUBDIRECTORY (backend/dbi)
  ADD_SUBDIRECTORY (backend/sql)
  ADD_SUBDIRECTORY (business/business-core/sql)
ENDIF (WITH_SQL)

ADD_SUBDIRECTORY (gnc)

