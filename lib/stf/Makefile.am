noinst_LTLIBRARIES = libgnc-stf.la

REALSRCS = stf-parse.c
REALHDRS = stf-parse.h

libgnc_stf_la_SOURCES = ${REALSRCS}
noinst_HEADERS = ${REALHDRS}

libgnc_stf_la_LIBADD = $(GOFFICE_LIBS)

AM_CPPFLAGS = $(GOFFICE_CFLAGS)

EXTRA_DIST = $(REALSRCS) $(REALHDRS)
