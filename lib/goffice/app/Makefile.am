SUBDIRS = 

noinst_LTLIBRARIES = libgoffice-app.la

libgoffice_app_la_SOURCES = \
	goffice-app.h			\
	go-doc.c			\
	go-doc.h			\
	go-doc-impl.h			\
	go-doc-control.c		\
	go-doc-control.h		\
	go-doc-control-impl.h

libgoffice_app_la_LIBADD = ${GLIB_LIBS}

AM_CFLAGS = ${GLIB_CFLAGS} ${GSF_CFLAGS}

include $(srcdir)/../goffice.mk
