gnucash_helpdir = $(GNC_HELPDIR)/pt_PT

GNUCASH_SGML_FILES = \
	bofa-mym.sgml \
	xacc-about.sgml \
	xacc-accountedit.sgml \
	xacc-acctypes.sgml \
	xacc-adjbalwin.sgml \
	xacc-apar.sgml \
	xacc-balancereport.sgml \
	xacc-balancesheet.sgml \
	xacc-chartofaccts.sgml \
	xacc-commodity.sgml \
	xacc-currencyhandling.sgml \
	xacc-dateinput.sgml \
	xacc-depreciation.sgml \
	xacc-doubleentry.sgml \
	xacc-euro.sgml \
	xacc-gpl.sgml \
	xacc-incomeexpense.sgml \
	xacc-locatingtxns.sgml \
	xacc-mainwin.sgml \
	xacc-newacctwin.sgml \
	xacc-pnl.sgml \
	xacc-portfolio-report.sgml \
	xacc-preferences.sgml \
	xacc-print-check.sgml \
	xacc-print.sgml \
	xacc-qif-import.sgml \
	xacc-quicken.sgml \
	xacc-recnwin.sgml \
	xacc-regwin-kbd.sgml \
	xacc-regwin.sgml \
	xacc-reports.sgml \
	xacc-tax-report.sgml \
	xacc-ticker.sgml \
	xacc-trans-report.sgml \
	xacc-txf-export-anomalies.sgml \
	xacc-txf-export.sgml \
	xacc-userdocs.sgml \
        gnucash.sgml \
        xacc-account-summary.sgml \
        xacc-asset-liability-barcharts.sgml \
        xacc-asset-liability-piecharts.sgml \
        xacc-common-report-options.sgml \
        xacc-dochack.sgml \
        xacc-gnome-mdi.sgml \
        xacc-gnucash-web-browser.sgml \
        xacc-income-expense-barcharts.sgml \
        xacc-income-expense-piecharts.sgml \
        xacc-multicolumn-view-reports.sgml \
        xacc-net-worth-barchart.sgml \
        xacc-repdev.sgml \
        xacc-scheme.sgml \
        xacc-stock-price-report.sgml \
        xacc-whats-new.sgml \
	xacc-y2k.sgml

gnucash_html_dir = $(top_srcdir)/doc/html/C

GNUCASH_HTML_FILES = \
	$(gnucash_html_dir)/xacc-tax-report-example.html

install-data-local: gnucash/index.html gnucash/help-search-index.db
	$(mkinstalldirs) $(DESTDIR)$(gnucash_helpdir)
	$(mkinstalldirs) $(DESTDIR)$(gnucash_helpdir)/image
	$(mkinstalldirs) $(DESTDIR)$(gnucash_helpdir)/stylesheet-images
	$(INSTALL_DATA) $(srcdir)/topic.dat \
			$(DESTDIR)$(gnucash_helpdir)/topic.dat
	-$(INSTALL_DATA) gnucash/help-search-index.db \
			 $(DESTDIR)$(gnucash_helpdir)/help-search-index.db
	-(for file in gnucash/*.html; do \
	    basefile=`basename $$file` \
	    $(INSTALL_DATA) $(srcdir)/$$file \
                            $(DESTDIR)$(gnucash_helpdir)/$$basefile; \
	  done)
	-(for file in gnucash/*.css; do \
	    basefile=`basename $$file` \
	    $(INSTALL_DATA) $(srcdir)/$$file \
                            $(DESTDIR)$(gnucash_helpdir)/$$basefile; \
	  done)
	-(for file in gnucash/stylesheet-images/*.gif; do \
	    basefile=`basename $$file` \
	    $(INSTALL_DATA) \
              $(srcdir)/$$file \
              $(DESTDIR)$(gnucash_helpdir)/stylesheet-images/$$basefile; \
	  done)


EXTRA_DIST = \
	$(GNUCASH_SGML_FILES) \
	$(GNUCASH_HTML_FILES) \
	topic.dat

manual-html: gnucash/index.html
manual-dvi: gnucash/gnucash.dvi
manual-ps: gnucash/gnucash.ps
manual-rtf: gnucash/gnucash.rtf
manual-all: manual-html manual-ps manual-rtf

all: manual-html

clean:
	rm -rf gnucash
	rm -rf gnucash.junk

gnucash/gnucash.dvi: $(GNUCASH_SGML_FILES)
	db2dvi gnucash.sgml

gnucash/gnucash.ps: $(GNUCASH_SGML_FILES)
	db2ps gnucash.sgml

gnucash/gnucash.rtf: $(GNUCASH_SGML_FILES)
	db2rtf gnucash.sgml

gnucash/index.html: $(GNUCASH_SGML_FILES) $(GNUCASH_HTML_FILES)
	-(db2html gnucash.sgml \
	  && cp $(GNUCASH_HTML_FILES) gnucash \
	  && mkdir -p gnucash/image \
	  && cp $(srcdir)/../C/image/*.png gnucash/image || exit 1) 

gnucash/help-search-index.db: $(GNUCASH_SGML_FILES) $(GNUCASH_HTML_FILES)
	-(cd gnucash && \
	  rm -f help-search-index.db && \
	  ../$(top_srcdir)/doc-tools/build-help-index \
            ../$(top_srcdir)/doc-tools/dbadd help-search-index.db *.html)

dist-hook:
	mkdir $(distdir)/gnucash
	mkdir $(distdir)/gnucash/image
	mkdir $(distdir)/gnucash/stylesheet-images
	cp gnucash/*.html $(distdir)/gnucash
	cp gnucash/*.css $(distdir)/gnucash
	cp gnucash/image/*.png $(distdir)/gnucash/image
	cp gnucash/stylesheet-images/*.gif $(distdir)/gnucash/stylesheet-images
