noinst_LTLIBRARIES = libgoffice-graph.la

libgoffice_graph_la_SOURCES = \
	gog-object.c			\
	gog-object-xml.c		\
	gog-styled-object.c		\
	gog-outlined-object.c		\
	gog-view.c			\
	\
	gog-graph.c			\
	gog-chart.c			\
	\
	gog-axis.c			\
	gog-axis-line.c			\
	gog-legend.c			\
	gog-label.c			\
	gog-grid.c			\
	gog-grid-line.c			\
	\
	gog-style.c			\
	gog-theme.c			\
	\
	gog-plot.c			\
	gog-plot-engine.c		\
	gog-series.c			\
	gog-error-bar.c			\
	gog-reg-curve.c			\
	gog-series-lines.c		\
	gog-data-allocator.c		\
	gog-data-set.c			\
	\
	gog-guru.c			\
	gog-renderer.c			\
	gog-renderer-pixbuf.c		\
	gog-control-foocanvas.c		\
	gog-renderer-gnome-print.c	\
	gog-renderer-svg.c

libgoffice_graph_ladir = $(goffice_include_dir)/graph
libgoffice_graph_la_HEADERS = \
	goffice-graph.h			\
	gog-object.h			\
	gog-object-xml.h		\
	gog-styled-object.h		\
	gog-outlined-object.h		\
	gog-view.h			\
	gog-graph.h			\
	gog-graph-impl.h		\
	gog-chart.h			\
	gog-chart-impl.h		\
	gog-axis.h			\
	gog-axis-line.h			\
	gog-axis-line-impl.h		\
	gog-legend.h			\
	gog-label.h			\
	gog-grid.h			\
	gog-grid-line.h			\
	gog-style.h			\
	gog-theme.h			\
	gog-plot.h			\
	gog-plot-impl.h			\
	gog-plot-engine.h		\
	gog-series.h			\
	gog-series-impl.h		\
	gog-error-bar.h			\
	gog-reg-curve.h			\
	gog-series-lines.h		\
	gog-data-allocator.h		\
	gog-data-set.h			\
	gog-guru.h			\
	gog-renderer.h			\
	gog-renderer-impl.h		\
	gog-renderer-pixbuf.h		\
	gog-control-foocanvas.h		\
	gog-renderer-gnome-print.h	\
	gog-renderer-svg.h

gladedir = $(goffice_datadir)/glade
dist_glade_DATA = \
	gog-guru.glade			\
	gog-guru-type-selector.glade	\
	gog-object-prefs.glade		\
	gog-chart-prefs.glade		\
	gog-style-prefs.glade		\
	gog-axis-prefs.glade		\
	gog-error-bar-prefs.glade	\
	gog-reg-curve-prefs.glade	\
	gog-reg-eqn-prefs.glade

include $(top_srcdir)/lib/goffice-0.0.4/goffice.mk
