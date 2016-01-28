noinst_LTLIBRARIES = libgnc-goffice.la

REALSRCS = go-optionmenu.c \
           go-charmap-sel.c \
           go-glib-extras.c
REALHDRS = go-optionmenu.h \
           go-charmap-sel.h \
           go-glib-extras.h

libgnc_goffice_la_SOURCES = ${REALSRCS}
noinst_HEADERS = ${REALHDRS}

libgnc_goffice_la_LIBADD = $(GTK_LIBS)

AM_CPPFLAGS = $(GTK_CFLAGS)

EXTRA_DIST = $(REALSRCS) $(REALHDRS)
