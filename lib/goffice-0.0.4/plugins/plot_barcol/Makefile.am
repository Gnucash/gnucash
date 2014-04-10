goffice_graph_barcoldir = $(goffice_plugindir)/plot_barcol
xmldir	 = $(goffice_graph_barcoldir)
gladedir = $(goffice_graph_barcoldir)

goffice_graph_barcol_LTLIBRARIES = barcol.la
barcol_la_LDFLAGS = -module $(GOFFICE_PLUGIN_FLAGS)
barcol_la_LIBADD = $(GOFFICE_PLUGIN_LIBADD)

barcol_la_SOURCES = gog-1.5d.c 		\
		    gog-1.5d.h		\
		    gog-line.c		\
		    gog-line.h		\
		    gog-barcol.c 	\
		    gog-barcol.h	\
		    gog-barcol-prefs.c	\
		    gog-dropbar.c	\
		    gog-dropbar.h	\
		    gog-minmax.c	\
		    gog-minmax.h	

xml_in_files = plugin.xml.in plot-types.xml.in
xml_DATA = $(xml_in_files:.xml.in=.xml)

@INTLTOOL_XML_RULE@

dist_glade_DATA = gog-barcol-prefs.glade gog-minmax-prefs.glade

# do not use the intl-tool stuff to merge the text back
# its simpler to just use gettext directly
plot-types.xml : plot-types.xml.in
	cp $< $@

EXTRA_DIST = $(xml_in_files)
DISTCLEANFILES = $(xml_in_files:.xml.in=.xml)

include $(top_srcdir)/lib/goffice-0.0.4/goffice-plugins.mk
