@INTLTOOL_XML_NOMERGE_RULE@

gschema_in_files = \
  org.gnucash.dialogs.import.hbci.gschema.xml.in

gsettings_SCHEMAS = \
  org.gnucash.dialogs.import.hbci.gschema.xml

# During the conversion from GConf to GSettings we are using
# the gconf backend for GSettings. Allow the more liberal
# GConf key names for GSettings so both tools see the
# same settings
# FIXME this has to be removed once GConf is dropped.
GLIB_COMPILE_SCHEMAS = glib-compile-schemas --allow-any-name

@GSETTINGS_RULES@

EXTRA_DIST = $(gschema_in_files)

CLEANFILES = $(gsettings_SCHEMAS)
