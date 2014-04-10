@INTLTOOL_SCHEMAS_RULE@

AM_CPPFLAGS = -I$(top_srcdir) $(LIBGSF_CFLAGS) $(LIBXML_CFLAGS)
LDADD = $(top_builddir)/gsf/libgsf-1.la $(LIBGSF_LIBS)

bin_PROGRAMS = gsf-office-thumbnailer

gsf_office_thumbnailer_SOURCES =	\
	main.c

schema_files = \
	gsf-office-thumbnailer.schemas.in

schema_DATA = gsf-office-thumbnailer.schemas

schemadir = $(GCONF_SCHEMA_FILE_DIR)

install-data-local:
if GCONF_SCHEMAS_INSTALL
	if test -z "$(DESTDIR)"; then \
	  for p in $(schema_DATA); do \
	    GCONF_CONFIG_SOURCE=$(GCONF_SCHEMA_CONFIG_SOURCE) \
	      $(GCONFTOOL) --makefile-install-rule $$p; \
	  done \
	fi
endif

EXTRA_DIST =         	\
	$(schema_files)  

DISTCLEANFILES = \
	$(schema_DATA)
