noinst_LTLIBRARIES = libgncglib.la

if HAVE_GLIB26
libgncglib_la_SOURCES = dummy.c
else
libgncglib_la_SOURCES = gkeyfile.c gkeyfile.h gutils26.c gutils26.h
endif

libgncglib_la_LIBADD = ${GLIB_LIBS}

AM_CFLAGS = ${GLIB_CFLAGS}
