SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-backend-file.la

AM_CFLAGS = \
  -I.. -I../.. \
  -I../../engine \
  -I../../gnc-module \
  ${GLIB_CFLAGS}

libgncmod_backend_file_la_SOURCES = \
  gncmod-backend-file.c \
  gnc-account-xml-v2.c \
  gnc-backend-file.c \
  gnc-commodity-xml-v2.c \
  gnc-freqspec-xml-v2.c \
  gnc-pricedb-xml-v2.c \
  gnc-schedxaction-xml-v2.c \
  gnc-transaction-xml-v2.c \
  io-example-account.c \
  io-gncbin-r.c \
  io-gncxml-gen.c \
  io-gncxml-v1.c \
  io-gncxml-v2.c \
  io-utils.c \
  sixtp-dom-generators.c \
  sixtp-dom-parsers.c \
  sixtp-stack.c \
  sixtp-to-dom-parser.c \
  sixtp-utils.c \
  sixtp.c

noinst_HEADERS = \
  gnc-xml.h \
  gnc-xml-helper.h \
  io-example-account.h \
  io-gncbin.h \
  io-gncxml-gen.h \
  io-gncxml.h \
  io-gncxml-v2.h \
  io-utils.h \
  sixtp-dom-generators.h \
  sixtp-dom-parsers.h \
  sixtp.h \
  sixtp-parsers.h \
  sixtp-stack.h \
  sixtp-utils.h

libgncmod_backend_file_la_LDFLAGS = -module
libgncmod_backend_file_la_LIBADD = \
   ${GLIB_LIBS} ${GNOME_XML_LIB}  \
   -L../../engine -L../../engine/.libs -lgncmod-engine
