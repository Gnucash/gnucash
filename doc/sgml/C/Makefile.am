gnucash_helpdir = $(datadir)/gnome/help/gnucash/C

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
	xacc-preferences2.sgml \
	xacc-print-check.sgml \
	xacc-print.sgml \
	xacc-project.sgml \
	xacc-qif-import.sgml \
	xacc-quicken.sgml \
	xacc-recnwin.sgml \
	xacc-regwin-kbd.sgml \
	xacc-regwin.sgml \
	xacc-reports.sgml \
	xacc-ticker.sgml \
	xacc-trans-report.sgml \
	xacc-userdocs.sgml \
	xacc-y2k.sgml

install-data-local: gnucash/index.html
	$(mkinstalldirs) $(DESTDIR)$(gnucash_helpdir)
	$(mkinstalldirs) $(DESTDIR)$(gnucash_helpdir)/image
	$(mkinstalldirs) $(DESTDIR)$(gnucash_helpdir)/stylesheet-images
#	$(INSTALL_DATA) $(srcdir)/topic.dat \
#			$(DESTDIR)$(gnucash_helpdir)/topic.dat
#	$(INSTALL_DATA) $(srcdir)/index.html $(DESTDIR)$(gnucash_helpdir)/
	for file in gnucash/*.html; do \
	  basefile=`basename $$file` \
	  $(INSTALL_DATA) $(srcdir)/$$file \
                          $(DESTDIR)$(gnucash_helpdir)/$$basefile; \
	done
	for file in gnucash/*.css; do \
	  basefile=`basename $$file` \
	  $(INSTALL_DATA) $(srcdir)/$$file \
                          $(DESTDIR)$(gnucash_helpdir)/$$basefile; \
	done
#	for file in gnucash/image/*.png; do \
#	  basefile=`basename $$file` \
#	  $(INSTALL_DATA) $(srcdir)/$$file \
#                          $(DESTDIR)$(gnucash_helpdir)/figs/$$basefile;\
#	done
	for file in gnucash/stylesheet-images/*.gif; do \
	  basefile=`basename $$file` \
	  $(INSTALL_DATA) \
            $(srcdir)/$$file \
            $(DESTDIR)$(gnucash_helpdir)/stylesheet-images/$$basefile; \
	done


EXTRA_DIST = \
	$(GNUCASH_SGML_FILES)
#	topic.dat

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

gnucash/index.html: $(GNUCASH_SGML_FILES)
	-(db2html gnucash.sgml)
#	  && mkdir -p users-guide/figs \
#	  && cp $(srcdir)/figs/*.png users-guide/figs || exit 1)


dist-hook:
	mkdir $(distdir)/gnucash
#	mkdir $(distdir)/gnucash/figs
	mkdir $(distdir)/gnucash/stylesheet-images
	cp gnucash/*.html $(distdir)/gnucash
	cp gnucash/*.css $(distdir)/gnucash
#	cp gnucash/figs/*.png $(distdir)/gnucash/figs
	cp gnucash/stylesheet-images/*.gif $(distdir)/gnucash/stylesheet-images
