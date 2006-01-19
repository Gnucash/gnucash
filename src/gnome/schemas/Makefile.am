schemadir   = @GCONF_SCHEMA_FILE_DIR@
schemas_in_files = \
  apps_gnucash_dialog_common.schemas.in \
  apps_gnucash_dialog_commodities.schemas.in \
  apps_gnucash_dialog_prices.schemas.in \
  apps_gnucash_dialog_reconcile.schemas.in \
  apps_gnucash_dialog_totd.schemas.in \
  apps_gnucash_general.schemas.in \
  apps_gnucash_warnings.schemas.in \
  apps_gnucash_window_pages_account_tree.schemas.in \
  apps_gnucash_window_pages_register.schemas.in
schema_DATA = $(schemas_in_files:.schemas.in=.schemas)

@INTLTOOL_SCHEMAS_RULE@

EXTRA_DIST = $(schemas_in_files)

CLEANFILES = $(schema_DATA)
