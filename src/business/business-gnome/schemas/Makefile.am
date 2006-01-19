schemadir   = @GCONF_SCHEMA_FILE_DIR@
schemas_in_files = \
  apps_gnucash_dialog_business_common.schemas.in
schema_DATA = $(schemas_in_files:.schemas.in=.schemas)

@INTLTOOL_SCHEMAS_RULE@

EXTRA_DIST = $(schemas_in_files)

CLEANFILES = $(schema_DATA)
