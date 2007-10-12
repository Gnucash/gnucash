noinst_LTLIBRARIES = libgnc-stf.la

REALSRCS = stf-parse.c
REALHDRS = stf-parse.h

libgnc_stf_la_SOURCES = ${REALSRCS}
noinst_HEADERS = ${REALHDRS}

libgnc_stf_la_LDFLAGS = $(pkg-config --libs libgoffice-0.3)

AM_CFLAGS = $(GOFFICE_CFLAGS)

EXTRA_DIST = $(REALSRCS) $(REALHDRS)
