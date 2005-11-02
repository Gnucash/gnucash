schemadir   = @GCONF_SCHEMA_FILE_DIR@
dist_schema_DATA = \
  apps_gnucash_dialog_common.schemas \
  apps_gnucash_dialog_commodities.schemas \
  apps_gnucash_dialog_prices.schemas \
  apps_gnucash_dialog_reconcile.schemas \
  apps_gnucash_dialog_totd.schemas \
  apps_gnucash_general.schemas \
  apps_gnucash_warnings.schemas \
  apps_gnucash_window_pages_account_tree.schemas \
  apps_gnucash_window_pages_register.schemas

install-data-local:
if GCONF_SCHEMAS_INSTALL
	-mkdir -p $(GCONF_SCHEMA_CONFIG_SOURCE_DIRONLY)
	-GCONF_CONFIG_SOURCE=$(GCONF_SCHEMA_CONFIG_SOURCE) gconftool-2 --makefile-install-rule $(srcdir)/$(dist_schema_DATA)
endif

uninstall-local:
if GCONF_SCHEMAS_INSTALL
	-GCONF_CONFIG_SOURCE=$(GCONF_SCHEMA_CONFIG_SOURCE) gconftool-2 --makefile-uninstall-rule $(srcdir)/$(dist_schema_DATA)
endif

