noinst_LTLIBRARIES = libgnc-glib.la

DUMMYSRCS = dummy.c
REALSRCS = gfileutils-2.8.c
REALHDRS = gfileutils-2.8.h
WIN32SRCS = gwin32-2.8.c
WIN32HDRS = gstdio-2.8.h gwin32-2.8.h

if HAVE_GLIB_2_8
libgnc_glib_la_SOURCES = ${DUMMYSRCS}
else
libgnc_glib_la_SOURCES = ${REALSRCS}
noinst_HEADERS = ${REALHDRS}
if PLATFORM_WIN32
libgnc_glib_la_SOURCES += $(WIN32SRCS)
noinst_HEADERS += $(WIN32HDRS)
endif
endif

libgnc_glib_la_LIBADD = ${GLIB26_LIBS}

AM_CFLAGS = ${GLIB_CFLAGS}

EXTRA_DIST = $(DUMMYSRCS) $(REALSRCS) $(REALHDRS) $(WIN32SRCS) $(WIN32HDRS)
