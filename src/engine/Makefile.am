# Engine Makefile.am file.

# don't build sql or RPC subdirs by default, unless you want hurt
SUBDIRS = ${SQL_DIR} ${RPC_DIR}

# but put them in the dist
DIST_SUBDIRS = sql rpc

# Build a libtool library, libhello.la for installation in libdir.
lib_LTLIBRARIES = libgncengine.la

# Build hell from main.c and libhello.la
libgncengine_la_SOURCES = \
  Account.c \
  Backend.c \
  DateUtils.c \
  GNCId.c \
  Group.c \
  NetIO.c \
  Query.c \
  Scrub.c \
  TransLog.c \
  Transaction.c \
  date.c \
  gnc-account-xml-v2.c \
  gnc-associate-account.c \
  gnc-book.c \
  gnc-book-file.c \
  gnc-commodity-xml-v2.c \
  gnc-commodity.c \
  gnc-engine-util.c \
  gnc-engine.c \
  gnc-event.c \
  gnc-numeric.c \
  gnc-pricedb-xml-v1.c \
  gnc-pricedb.c \
  gnc-transaction-xml-v2.c \
  guid.c \
  io-example-account.c \
  io-gncbin-r.c \
  io-gncxml-gen.c \
  io-gncxml-v1.c \
  io-gncxml-v2.c \
  io-utils.c \
  kvp_frame.c \
  md5.c \
  sixtp-dom-generators.c \
  sixtp-dom-parsers.c \
  sixtp-stack.c \
  sixtp-to-dom-parser.c \
  sixtp-utils.c \
  sixtp.c \
  FreqSpec.c \
  SchedXaction.c \
  gnc-freqspec-xml-v2.c \
  gnc-schedxaction-xml-v2.c

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
  gnc-associate-account.h \
  gnc-book-p.h \
  gnc-book.h \
  gnc-commodity.h \
  gnc-common.h \
  gnc-engine-util.h \
  gnc-engine.h \
  gnc-event-p.h \
  gnc-event.h \
  gnc-numeric.h \
  gnc-pricedb-p.h \
  gnc-pricedb.h \
  gnc-xml-helper.h \
  gnc-xml.h \
  guid.h \
  io-example-account.h \
  io-gncbin.h \
  io-gncxml-gen.h \
  io-gncxml-v2.h \
  io-gncxml.h \
  io-utils.h \
  kvp_frame.h \
  md5.h \
  sixtp-dom-generators.h \
  sixtp-dom-parsers.h \
  sixtp-parsers.h \
  sixtp-stack.h \
  sixtp-utils.h \
  sixtp.h \
  FreqSpec.h \
  SchedXaction.h

EXTRA_DIST = \
  .cvsignore \
  README.query-api \
  design.txt \
  extensions.txt \
  kvp_doc.txt

CFLAGS = @CFLAGS@ ${GLIB_CFLAGS} ${GNOME_XML_CFLAGS} ${GHTTP_CFLAGS}

LDADD = ${GNOME_XML_LIB} ${GLIB_LIBS}

INCLUDES = -I..
