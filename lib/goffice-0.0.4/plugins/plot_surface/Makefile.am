goffice_graph_surfacedir = $(goffice_plugindir)/plot_surface
xmldir	 = $(goffice_graph_surfacedir)
gladedir = $(goffice_graph_surfacedir)

goffice_graph_surface_LTLIBRARIES = surface.la
surface_la_LDFLAGS = -module $(GOFFICE_PLUGIN_FLAGS)
surface_la_LIBADD = $(GOFFICE_PLUGIN_LIBADD) 

surface_la_SOURCES = \
	gog-surface.c		\
	gog-surface.h		\
	xl-surface.c		\
	xl-surface.h		\
	gog-contour-prefs.c

xml_in_files = plugin.xml.in plot-types.xml.in
xml_DATA = $(xml_in_files:.xml.in=.xml)

@INTLTOOL_XML_RULE@

dist_glade_DATA = gog-contour-prefs.glade

# do not use the intl-tool stuff to merge the text back
# its simpler to just use gettext directly
plot-types.xml : plot-types.xml.in
	cp $< $@

EXTRA_DIST = $(xml_in_files)
DISTCLEANFILES = $(xml_in_files:.xml.in=.xml)

include $(top_srcdir)/lib/goffice-0.0.4/goffice-plugins.mk
