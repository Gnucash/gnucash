SUBDIRS = . test

#Now a shared GModule, not a package library.
lib_LTLIBRARIES = libgnc-backend-file.la

AM_CFLAGS = \
  -I.. -I../.. \
  -DLOCALE_DIR=\""$(datadir)/locale"\" \
  -I${top_srcdir}/src/backend \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/core-utils\
  -I${top_srcdir}/lib/libc\
  ${XML_CFLAGS} \
  ${QOF_CFLAGS} \
  ${GLIB_CFLAGS} \
  ${GCONF_CFLAGS}

libgnc_backend_file_la_SOURCES = \
  gnc-account-xml-v2.c \
  gnc-backend-file.c \
  gnc-book-xml-v2.c \
  gnc-budget-xml-v2.c \
  gnc-commodity-xml-v2.c \
  gnc-freqspec-xml-v2.c \
  gnc-lot-xml-v2.c \
  gnc-pricedb-xml-v2.c \
  gnc-recurrence-xml-v2.c \
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
  gnc-backend-file.h \
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

libgnc_backend_file_la_LDFLAGS = -module
libgnc_backend_file_la_LIBADD = \
   ${GLIB_LIBS} ${GCONF_LIBS} ${XML_LIBS} \
   ${top_builddir}/src/engine/libgncmod-engine.la \
   ${top_builddir}/src/core-utils/libcore-utils.la \
   ${QOF_LIBS}
