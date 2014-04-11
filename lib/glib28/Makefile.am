noinst_LTLIBRARIES = libgnc-glib.la

DUMMYSRCS = dummy.c
REALSRCS = gfileutils-2.8.c
REALHDRS = gfileutils-2.8.h gstdio-2.8.h

if HAVE_GLIB_2_8
libgnc_glib_la_SOURCES = ${DUMMYSRCS}
else
libgnc_glib_la_SOURCES = ${REALSRCS}
noinst_HEADERS = ${REALHDRS}
endif

libgnc_glib_la_LIBADD = ${GLIB26_LIBS}

AM_CPPFLAGS = ${GLIB_CFLAGS}

EXTRA_DIST = $(DUMMYSRCS) $(REALSRCS) $(REALHDRS)
