schemadir   = @GCONF_SCHEMA_FILE_DIR@
schema_DATA = \
  apps_gnucash_dialog_commodities.schemas \
  apps_gnucash_dialog_prices.schemas

install-data-local:
if GCONF_SCHEMAS_INSTALL
	-GCONF_CONFIG_SOURCE=$(GCONF_SCHEMA_CONFIG_SOURCE) gconftool-2 --makefile-install-rule $(srcdir)/$(schema_DATA)
endif

uninstall-local:
if GCONF_SCHEMAS_INSTALL
	-GCONF_CONFIG_SOURCE=$(GCONF_SCHEMA_CONFIG_SOURCE) gconftool-2 --makefile-uninstall-rule $(srcdir)/$(schema_DATA)
endif

