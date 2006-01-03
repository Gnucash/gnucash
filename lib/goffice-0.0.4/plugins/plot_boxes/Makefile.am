goffice_graph_boxplotdir = $(goffice_plugindir)/plot_boxes
xmldir	 = $(goffice_graph_boxplotdir)
gladedir = $(goffice_graph_boxplotdir)

goffice_graph_boxplot_LTLIBRARIES = boxplot.la
boxplot_la_LDFLAGS = -module $(GOFFICE_PLUGIN_FLAGS)
boxplot_la_LIBADD = $(GOFFICE_PLUGIN_LIBADD)

boxplot_la_SOURCES = gog-boxplot.c	\
		    gog-boxplot.h

xml_in_files = plugin.xml.in plot-types.xml.in
xml_DATA = $(xml_in_files:.xml.in=.xml)

@INTLTOOL_XML_RULE@

dist_glade_DATA = gog-boxplot-prefs.glade

# do not use the intl-tool stuff to merge the text back
# its simpler to just use gettext directly
plot-types.xml : plot-types.xml.in
	cp $< $@

EXTRA_DIST = $(xml_in_files)
DISTCLEANFILES = $(xml_in_files:.xml.in=.xml)

include $(top_srcdir)/lib/goffice-0.0.4/goffice-plugins.mk
