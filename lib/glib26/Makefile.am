noinst_LTLIBRARIES = libgncglib.la

DUMMYSRCS=dummy.c
REALSRCS=gkeyfile.c gkeyfile.h gutils26.c gutils26.h

if HAVE_GLIB26
libgncglib_la_SOURCES = $(DUMMYSRCS)
else
libgncglib_la_SOURCES = $(REALSRCS)
endif

libgncglib_la_LIBADD = ${GLIB_LIBS}

AM_CFLAGS = ${GLIB_CFLAGS}

EXTRA_DIST=$(DUMMYSRCS) $(REALSRCS)
