goffice_graph_piedir = $(goffice_plugindir)/plot_pie
xmldir	 = $(goffice_graph_piedir)
gladedir = $(goffice_graph_piedir)

goffice_graph_pie_LTLIBRARIES = pie.la
pie_la_LDFLAGS = -module $(GOFFICE_PLUGIN_FLAGS)
pie_la_LIBADD = $(GOFFICE_PLUGIN_LIBADD)

pie_la_SOURCES = \
	gog-pie.c		\
	gog-pie.h		\
	gog-pie-prefs.c

xml_in_files = plugin.xml.in plot-types.xml.in
xml_DATA = $(xml_in_files:.xml.in=.xml)

@INTLTOOL_XML_RULE@

dist_glade_DATA = gog-pie-prefs.glade gog-ring-prefs.glade gog-pie-series.glade

# do not use the intl-tool stuff to merge the text back
# its simpler to just use gettext directly
plot-types.xml : plot-types.xml.in
	cp $< $@

EXTRA_DIST = $(xml_in_files)
DISTCLEANFILES = $(xml_in_files:.xml.in=.xml)

include $(top_srcdir)/lib/goffice-0.0.4/goffice-plugins.mk
