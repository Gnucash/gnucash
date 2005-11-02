schemadir   = @GCONF_SCHEMA_FILE_DIR@
dist_schema_DATA = \
  apps_gnucash_import_generic_matcher.schemas

install-data-local:
if GCONF_SCHEMAS_INSTALL
	-mkdir -p $(GCONF_SCHEMA_CONFIG_SOURCE_DIRONLY)
	-GCONF_CONFIG_SOURCE=$(GCONF_SCHEMA_CONFIG_SOURCE) gconftool-2 --makefile-install-rule $(srcdir)/$(dist_schema_DATA)
endif

uninstall-local:
if GCONF_SCHEMAS_INSTALL
	-GCONF_CONFIG_SOURCE=$(GCONF_SCHEMA_CONFIG_SOURCE) gconftool-2 --makefile-uninstall-rule $(srcdir)/$(dist_schema_DATA)
endif

