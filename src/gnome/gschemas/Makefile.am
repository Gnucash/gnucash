@INTLTOOL_XML_NOMERGE_RULE@

gschema_in_files = \
  org.gnucash.dialogs.gschema.xml.in \
  org.gnucash.dialogs.commodities.gschema.xml.in \
  org.gnucash.dialogs.checkprinting.gschema.xml.in \
  org.gnucash.dialogs.reconcile.gschema.xml.in \
  org.gnucash.dialogs.sxs.gschema.xml.in \
  org.gnucash.dialogs.totd.gschema.xml.in \
  org.gnucash.gschema.xml.in \
  org.gnucash.window.pages.account.tree.gschema.xml.in \
  org.gnucash.window.pages.gschema.xml.in

gsettings_SCHEMAS = $(gschema_in_files:.gschema.xml.in=.gschema.xml)

#GLIB_COMPILE_SCHEMAS = glib-compile-schemas --allow-any-name

@GSETTINGS_RULES@

EXTRA_DIST = $(gschema_in_files)

CLEANFILES = $(gsettings_SCHEMAS)
