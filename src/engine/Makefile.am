
# Engine Makefile.am file.

# Build a libtool library, libhello.la for installation in libdir.
lib_LTLIBRARIES = libgncengine.la

# Build hell from main.c and libhello.la
libgncengine_la_SOURCES = \
  AccInfo.c \
  Account.c \
  Backend.c \
  DateUtils.c \
  FileIO.c \
  Group.c \
  LedgerUtils.c \
  Query.c \
  Queue.c \
  Scrub.c \
  Session.c \
  Transaction.c \
  TransLog.c \
  date.c \
  util.c \
  GNCId.c \
  guid.c \
  md5.c

libgncengine_la_LDFLAGS = -version-info 1:1:1

CFLAGS = @CFLAGS@ ${GLIB_CFLAGS}

INCLUDES = -I..
