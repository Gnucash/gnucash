# Engine Makefile.am file.

SUBDIRS = sql

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
  md5.c \
  kvp_frame.c

libgncengine_la_LDFLAGS = -version-info 1:1:1

noinst_HEADERS = \
  AccInfo.h \
  AccInfoP.h \
  Account.h \
  AccountP.h \
  BackendP.h \
  DateUtils.h \
  FileIO.h \
  FileIOP.h \
  GNCId.h \
  GNCIdP.h \
  Group.h \
  GroupP.h \
  Query.h \
  Queue.h \
  Scrub.h \
  Session.h \
  TransLog.h \
  Transaction.h \
  TransactionP.h \
  date.h \
  gnc-common.h \
  guid.h \
  md5.h \
  util.h \
  kvp_frame.h

EXTRA_DIST = \
  .cvsignore \
  README.query-api \
  design.txt \
  extensions.txt

CFLAGS = @CFLAGS@ ${GLIB_CFLAGS} ${GUILE_COMPILE_ARGS}

INCLUDES = -I..
