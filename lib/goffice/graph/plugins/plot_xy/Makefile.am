goffice_graph_xydir = $(gnumeric_plugindir)/plot_xy
xmldir	 = $(goffice_graph_xydir)
gladedir = $(goffice_graph_xydir)

goffice_graph_xy_LTLIBRARIES = xy.la
xy_la_LDFLAGS = -module $(GOFFICE_PLUGIN_FLAGS)
xy_la_SOURCES = \
	gog-xy.c		\
	gog-xy.h		\
	gog-bubble-prefs.c

xml_in_files = plugin.xml.in plot-types.xml.in
xml_DATA = $(xml_in_files:.xml.in=.xml) gog-bubble-prefs.glade

@INTLTOOL_XML_RULE@

glade_DATA = gog-bubble-prefs.glade

# do not use the intl-tool stuff to merge the text back
# its simpler to just use gettext directly
plot-types.xml : plot-types.xml.in
	cp $< $@

EXTRA_DIST	= $(xml_in_files) $(glade_DATA)
DISTCLEANFILES	= $(xml_in_files:.xml.in=.xml)

include $(srcdir)/../../../goffice-plugins.mk
