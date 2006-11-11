SUBDIRS = .

# Now a shared library AND a GModule
lib_LTLIBRARIES = libgnc-backend-gda.la

AM_CFLAGS = \
  -I.. -I../.. \
  -DLOCALE_DIR=\""$(datadir)/locale"\" \
  -I${top_srcdir}/src/backend \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/core-utils\
  -I${top_srcdir}/lib/libc\
  ${QOF_CFLAGS} \
  ${GLIB_CFLAGS} \
  ${GCONF_CFLAGS} \
  ${LIBGDA_CFLAGS}

libgnc_backend_gda_la_SOURCES = \
  gnc-backend-gda.c \
  gnc-account-gda.c \
  gnc-budget-gda.c \
  gnc-commodity-gda.c \
  gnc-price-gda.c \
  gnc-slots-gda.c \
  gnc-transaction-gda.c

#noinst_HEADERS = \
#  gnc-backend-file.h \
#  gnc-xml.h \
#  gnc-xml-helper.h \
#  io-example-account.h \
#  io-gncxml-gen.h \
#  io-gncxml.h \
#  io-gncxml-v2.h \
#  io-utils.h \
#  sixtp-dom-generators.h \
#  sixtp-dom-parsers.h \
#  sixtp.h \
#  sixtp-parsers.h \
#  sixtp-stack.h \
#  sixtp-utils.h

libgnc_backend_gda_la_LDFLAGS = -module -avoid-version
libgnc_backend_gda_la_LIBADD = \
	${LIBGDA_LIBS}
