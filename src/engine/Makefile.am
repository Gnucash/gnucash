# Engine Makefile.am file.

# don't build sql subdir by default, unless you want hurt
SUBDIRS = ${SQL_DIR}

# but put it in the dist
DIST_SUBDIRS = sql

# Build a libtool library, libhello.la for installation in libdir.
lib_LTLIBRARIES = libgncengine.la

# Build hell from main.c and libhello.la
libgncengine_la_SOURCES = \
  Account.c \
  Backend.c \
  DateUtils.c \
  Group.c \
  NetIO.c \
  Query.c \
  Scrub.c \
  Transaction.c \
  TransLog.c \
  date.c \
  GNCId.c \
  gnc-account-xml-v2.c \
  gnc-commodity-xml-v2.c \
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
  gnc-numeric.c \
  gnc-pricedb.c \
  gnc-pricedb-xml-v1.c \
  sixtp-dom-generators.c \
  sixtp-dom-parsers.c \
  sixtp-kvp-parser.c \
  sixtp-stack.c \
  sixtp-utils.c \
  sixtp-xml-write-utils.c \
  sixtp-to-dom-parser.c \
  sixtp.c \
  Account-xml-parser-v1.c \
  Commodity-xml-parser-v1.c \
  Ledger-xml-parser-v1.c \
  Query-xml-parser-v1.c \
  Transaction-xml-parser-v1.c

libgncengine_la_LDFLAGS = -version-info 2:3:1

noinst_HEADERS = \
  Account.h \
  AccountP.h \
  Backend.h \
  BackendP.h \
  DateUtils.h \
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
  gnc-numeric.h \
  gnc-pricedb.h \
  gnc-xml-helper.h \
  gnc-xml.h \
  sixtp-dom-generators.h \
  sixtp-dom-parsers.h \
  sixtp-parsers.h \
  sixtp-writers.h \
  sixtp-stack.h \
  sixtp-utils.h \
  sixtp-xml-write-utils.h \
  sixtp.h

EXTRA_DIST = \
  .cvsignore \
  README.query-api \
  design.txt \
  extensions.txt \
  kvp_doc.txt

CFLAGS = @CFLAGS@ ${GLIB_CFLAGS} ${GNOME_XML_CFLAGS} ${GHTTP_CFLAGS}

LDADD = ${GNOME_XML_LIB}

INCLUDES = -I..
