@INTLTOOL_XML_NOMERGE_RULE@

gschema_in_files = \
  org.gnucash.dialogs.gschema.xml.in \
  org.gnucash.dialogs.commodities.gschema.xml.in \
  org.gnucash.dialogs.prices.gschema.xml.in \
  org.gnucash.dialogs.checkprinting.gschema.xml.in \
  org.gnucash.dialogs.reconcile.gschema.xml.in \
  org.gnucash.dialogs.sxs.gschema.xml.in \
  org.gnucash.dialogs.totd.gschema.xml.in \
  org.gnucash.general.gschema.xml.in \
  org.gnucash.warnings.gschema.xml.in \
  org.gnucash.window.pages.account.tree.gschema.xml.in \
  org.gnucash.window.pages.gschema.xml.in \
  org.gnucash.window.pages.register.gschema.xml.in

gsettings_SCHEMAS = \
  org.gnucash.dialogs.gschema.xml \
  org.gnucash.dialogs.commodities.gschema.xml \
  org.gnucash.dialogs.prices.gschema.xml \
  org.gnucash.dialogs.checkprinting.gschema.xml \
  org.gnucash.dialogs.reconcile.gschema.xml \
  org.gnucash.dialogs.sxs.gschema.xml \
  org.gnucash.dialogs.totd.gschema.xml \
  org.gnucash.general.gschema.xml \
  org.gnucash.warnings.gschema.xml \
  org.gnucash.window.pages.account.tree.gschema.xml \
  org.gnucash.window.pages.gschema.xml \
  org.gnucash.window.pages.register.gschema.xml

# During the conversion from GConf to GSettings we are using
# the gconf backend for GSettings. Allow the more liberal
# GConf key names for GSettings so both tools see the
# same settings
# FIXME this has to be removed once GConf is dropped.
GLIB_COMPILE_SCHEMAS = glib-compile-schemas --allow-any-name

@GSETTINGS_RULES@

EXTRA_DIST = $(gschema_in_files)

CLEANFILES = $(gsettings_SCHEMAS)
