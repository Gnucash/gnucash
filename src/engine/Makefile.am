# Engine Makefile.am file.

SUBDIRS = sql

# Build a libtool library, libhello.la for installation in libdir.
lib_LTLIBRARIES = libgncengine.la

# Build hell from main.c and libhello.la
libgncengine_la_SOURCES = \
  Account.c \
  Backend.c \
  DateUtils.c \
  FileIO.c \
  Group.c \
  NetIO.c \
  Query.c \
  Scrub.c \
  Transaction.c \
  TransLog.c \
  date.c \
  GNCId.c \
  guid.c \
  io-gncbin-r.c \
  io-gncxml-r.c \
  io-gncxml-w.c \
  md5.c \
  kvp_frame.c \
  gnc-associate-account.c \
  gnc-book.c \
  gnc-commodity.c \
  gnc-engine.c \
  gnc-engine-util.c \
  gnc-event.c \
  gnc-numeric.c

libgncengine_la_LDFLAGS = -version-info 2:1:1

noinst_HEADERS = \
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
  NetIO.h \
  Query.h \
  Scrub.h \
  TransLog.h \
  Transaction.h \
  TransactionP.h \
  date.h \
  gnc-common.h \
  guid.h \
  io-gncbin.h \
  io-gncxml.h \
  md5.h \
  kvp_frame.h \
  gnc-associate-account.h \
  gnc-book.h \
  gnc-commodity.h \
  gnc-engine.h \
  gnc-engine-util.h \
  gnc-event.h \
  gnc-event-p.h \
  gnc-numeric.h

EXTRA_DIST = \
  .cvsignore \
  README.query-api \
  README.gnc-numeric \
  design.txt \
  extensions.txt \
  io-gncxml-version1.dtd \
  kvp_doc.txt

CFLAGS = @CFLAGS@ ${GLIB_CFLAGS} ${GNOME_XML_CFLAGS} ${GHTTP_CFLAGS}

LDADD = ${GNOME_XML_LIB}

INCLUDES = -I..
