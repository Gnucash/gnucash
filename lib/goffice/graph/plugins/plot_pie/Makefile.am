goffice_graph_piedir = $(gnumeric_plugindir)/plot_pie
xmldir	 = $(goffice_graph_piedir)
gladedir = $(goffice_graph_piedir)

goffice_graph_pie_LTLIBRARIES = pie.la
pie_la_LDFLAGS = -module $(GOFFICE_PLUGIN_FLAGS)
pie_la_SOURCES = \
	gog-pie.c		\
	gog-pie.h		\
	gog-pie-prefs.c

xml_in_files = plugin.xml.in plot-types.xml.in
xml_DATA = $(xml_in_files:.xml.in=.xml) gog-pie-prefs.glade gog-ring-prefs.glade gog-pie-series.glade

@INTLTOOL_XML_RULE@

glade_DATA = gog-pie-prefs.glade gog-ring-prefs.glade gog-pie-series.glade

# do not use the intl-tool stuff to merge the text back
# its simpler to just use gettext directly
plot-types.xml : plot-types.xml.in
	cp $< $@

EXTRA_DIST = $(xml_in_files) $(glade_DATA)
DISTCLEANFILES = $(xml_in_files:.xml.in=.xml)

# AM_CFLAGS = ${GLIB_CFLAGS} ${XML_CFLAGS} ${GSF_CFLAGS} ${ART_CFLAGS} ${GNOME_CFLAGS} ${GDK_PIXBUF_CLFAGS} ${GLADE_CFLAGS}

include $(srcdir)/../../../goffice-plugins.mk

