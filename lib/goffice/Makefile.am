SUBDIRS = app graph utils gui-utils drawing pixmaps split cut-n-paste 

AM_CFLAGS = $(GLIB_CFLAGS) $(ART_CFLAGS) $(GNOME_CFLAGS) $(GSF_CFLAGS) $(PRINT_CFLAGS)

noinst_LTLIBRARIES = libgoffice.la

BUILT_SOURCES = \
	paths.h

libgoffice_la_LIBADD = 				\
	utils/libgoffice-utils.la		\
	app/libgoffice-app.la			\
	gui-utils/libgoffice-gui-utils.la	\
	graph/libgoffice-graph.la		\
	drawing/libgoffice-drawing.la           \
	split/libgoffice-split.la          	\
	split/widgets/libgoffice-split-widgets.la \
	cut-n-paste/pcre/libpcre.la		\
	${GLIB_LIBS}

libgoffice_la_SOURCES = 	\
	$(BUILT_SOURCES)        \
	goffice.c		\
	goffice.h		\
	goffice-config.h	\
	split.h			\
	split.c

EXTRA_DIST = goffice.mk goffice-plugins.mk paths.h.in

paths.h: paths.h.in ${top_builddir}/config.status
	rm -f $@.tmp
	sed < $< > $@.tmp \
		-e 's:@-GNC_LIBDIR-@:${GNC_LIBDIR}:g'
	mv $@.tmp $@

include $(srcdir)/goffice.mk
