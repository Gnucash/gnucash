@INTLTOOL_XML_NOMERGE_RULE@

gschema_in_files = \
  org.gnucash.dialogs.import.generic.gschema.xml.in

gsettings_SCHEMAS = $(gschema_in_files:.gschema.xml.in=.gschema.xml)

#GLIB_COMPILE_SCHEMAS = glib-compile-schemas --allow-any-name

@GSETTINGS_RULES@

EXTRA_DIST = $(gschema_in_files)

CLEANFILES = $(gsettings_SCHEMAS)
