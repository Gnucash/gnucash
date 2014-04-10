SUBDIRS = plugins

# SUBDIRS += tests
## Don't put comment on an assignment line!  It can trigger an automake bug.

noinst_LTLIBRARIES = libgoffice-graph.la

AM_CFLAGS = ${GLIB_CFLAGS} ${GSF_CFLAGS} ${ART_CFLAGS} ${GLADE_CFLAGS} ${GNOME_CFLAGS} ${PRINT_CFLAGS}

libgoffice_graph_la_SOURCES = \
	goffice-graph.h			\
	gog-object.c			\
	gog-object.h			\
	gog-object-xml.c		\
	gog-object-xml.h		\
	gog-styled-object.c		\
	gog-styled-object.h		\
	gog-outlined-object.c		\
	gog-outlined-object.h		\
	gog-view.c			\
	gog-view.h			\
	\
	gog-graph.c			\
	gog-graph.h			\
	gog-graph-impl.h		\
	gog-chart.c			\
	gog-chart.h			\
	gog-chart-impl.h		\
	\
	gog-axis.c			\
	gog-axis.h			\
	gog-legend.c			\
	gog-legend.h			\
	gog-label.c			\
	gog-label.h			\
	gog-grid.c			\
	gog-grid.h			\
	gog-grid-line.c			\
	gog-grid-line.h			\
	\
	gog-style.c			\
	gog-style.h			\
	gog-theme.c			\
	gog-theme.h			\
	\
	gog-plot.c			\
	gog-plot.h			\
	gog-plot-impl.h			\
	gog-plot-engine.h		\
	gog-plot-engine.c		\
	gog-series.c			\
	gog-series.h			\
	gog-series-impl.h		\
	gog-error-bar.c			\
	gog-error-bar.h			\
	gog-data-allocator.c		\
	gog-data-allocator.h		\
	gog-data-set.c			\
	gog-data-set.h			\
	\
	go-data.c			\
	go-data.h			\
	go-data-impl.h			\
	go-data-simple.c		\
	go-data-simple.h		\
	\
	gog-renderer.c			\
	gog-renderer.h			\
	gog-renderer-impl.h		\
	gog-renderer-pixbuf.c		\
	gog-renderer-pixbuf.h		\
	gog-renderer-gnome-print.c	\
	gog-renderer-gnome-print.h	\
	gog-renderer-svg.c		\
	gog-renderer-svg.h

#
#	gog-guru.c			\
#	gog-guru.h			\
#	gog-control-foocanvas.c		\
#	gog-control-foocanvas.h		\
#

#graph_glade_DATA = 			\
#	gog-guru.glade			\
#	gog-guru-type-selector.glade	\
#	gog-style-prefs.glade		\
#	gog-axis-prefs.glade		\
#	gog-error-bar-prefs.glade

# EXTRA_DIST = $(graph_glade_DATA)

include $(srcdir)/../goffice.mk
