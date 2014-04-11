schemadir   = @GCONF_SCHEMA_FILE_DIR@
schemas_in_files = \
  apps_gnucash_dialog_hbci.schemas.in
schema_DATA = $(schemas_in_files:.schemas.in=.schemas)

@INTLTOOL_SCHEMAS_RULE@

EXTRA_DIST = $(schemas_in_files)

CLEANFILES = $(schema_DATA)

install-data-local:
if GCONF_SCHEMAS_INSTALL
	-mkdir -p $(DESTDIR)$(GCONF_SCHEMA_CONFIG_SOURCE_DIRONLY)
	GCONF_CONFIG_SOURCE=$(GCONF_SCHEMA_CONFIG_SOURCE) $(GCONFTOOL) --makefile-install-rule $(schema_DATA)
endif

uninstall-local:
if GCONF_SCHEMAS_INSTALL
	GCONF_CONFIG_SOURCE=$(GCONF_SCHEMA_CONFIG_SOURCE) $(GCONFTOOL) --makefile-uninstall-rule $(schema_DATA)
endif
