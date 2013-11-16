@INTLTOOL_XML_NOMERGE_RULE@

gschema_in_files = \
  org.gnucash.history.gschema.xml.in \
  org.gnucash.warnings.gschema.xml.in

gsettings_SCHEMAS = $(gschema_in_files:.gschema.xml.in=.gschema.xml)

@GSETTINGS_RULES@

CLEANFILES = $(gsettings_SCHEMAS)
