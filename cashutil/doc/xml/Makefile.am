SUBDIRS= .

EXTRA_DIST = \
	cashutil.docbook \
	cashutil.xml

all:
XML_CATALOG_FILES=$(XML_CATALOG_FILES) $(XSLTPROC) $(XSLTPROC_FLAGS) -o $@ $(top_srcdir)/doc/xml/docbook.xsl $<
	$(XSLTPROC) $(XSLTPROC_FLAGS) -o $(top_srcdir)/doc/man/ \
	$(XSLTROOT)/docbook.xsl \
	$(top_srcdir)/doc/xml/cashutil.docbook

distclean-local:
	$(RM) -f xml/catalog.xml
	$(RM) -f xml/docbook.xsl
