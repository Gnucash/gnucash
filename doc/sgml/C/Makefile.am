#SUBDIRS = 

gnucash_helpdir = $(GNC_HELPDIR)/C

GNUCASH_SGML_FILES = \
	help.sgml \
	guide.sgml

install-data-local: gnucash/help.html gnucash/index.html gnucash/help-search-index.db
	$(mkinstalldirs) $(DESTDIR)$(gnucash_helpdir)
	$(mkinstalldirs) $(DESTDIR)$(gnucash_helpdir)/figures
	$(mkinstalldirs) $(DESTDIR)$(gnucash_helpdir)/stylesheet-images
	$(INSTALL_DATA) $(srcdir)/topic.dat \
			$(DESTDIR)$(gnucash_helpdir)/topic.dat
	-$(INSTALL_DATA) $(srcdir)/help-search-index.db \
			 $(DESTDIR)$(gnucash_helpdir)/help-search-index.db
	-(for file in help/*.html; do \
	    basefile=`basename $$file` \
	    $(INSTALL_DATA) $(srcdir)/$$file \
                            $(DESTDIR)$(gnucash_helpdir)/$$basefile; \
	  done)
	-(for file in guide/*.html; do \
	    basefile=`basename $$file` \
	    $(INSTALL_DATA) $(srcdir)/$$file \
                            $(DESTDIR)$(gnucash_helpdir)/$$basefile; \
	  done)
	-(for file in guide/*.css; do \
	    basefile=`basename $$file` \
	    $(INSTALL_DATA) $(srcdir)/$$file \
                            $(DESTDIR)$(gnucash_helpdir)/$$basefile; \
	  done)
	-(for file in guide/stylesheet-images/*.gif; do \
	    basefile=`basename $$file` \
	    $(INSTALL_DATA) \
              $(srcdir)/$$file \
              $(DESTDIR)$(gnucash_helpdir)/stylesheet-images/$$basefile; \
	  done)

uninstall-local:
	-rm -f $(DESTDIR)$(gnucash_helpdir)/topic.dat;
	-rm -f $(DESTDIR)$(gnucash_helpdir)/help-search-index.db;
	-(for file in $(DESTDIR)$(gnucash_helpdir)/*.html; do \
	    echo " rm -f $$file"; \
	    rm -f $$file; \
	  done)
	-(for file in $(DESTDIR)$(gnucash_helpdir)/*.css; do \
	    echo " rm -f $$file"; \
	    rm -f $$file; \
	  done)
	-(for file in $(DESTDIR)$(gnucash_helpdir)/stylesheet-images/*.gif; do \
	    echo " rm -f $$file"; \
	    rm -f $$file; \
	  done)

EXTRA_DIST = \
	topic.dat

clean:
	rm -rf guide
	rm -rf guide.junk
	rm -rf help
	rm -rf help.junk

gnucash/index.html: $(GNUCASH_SGML_FILES) $(GNUCASH_HTML_FILES)
	-(db2html -V %root-filename%=index -u ${srcdir}/guide.sgml || exit 1) 

gnucash/help.html: $(GNUCASH_SGML_FILES) $(GNUCASH_HTML_FILES)
	-(db2html -V %root-filename%=help -u ${srcdir}/help.sgml || exit 1)
	  
gnucash/help-search-index.db: $(GNUCASH_SGML_FILES) $(GNUCASH_HTML_FILES)
	-(rm -f help-search-index.db && \
	  $(top_srcdir)/doc-tools/build-help-index \
            $(top_srcdir)/doc-tools/dbadd help-search-index.db help/*.html \
	    guide/*.html || exit 1)

dist-hook: gnucash/help.html gnucash/index.html gnucash/help-search-index.db
	mkdir $(distdir)/gnucash
	mkdir $(distdir)/gnucash/figures
	mkdir $(distdir)/gnucash/stylesheet-images
	-(if test -f guide.sgml; then dir=.; \
	  else dir="${srcdir}"; fi; \
	  for file in $(GNUCASH_SGML_FILES); do \
	    cp $${dir}/$$file $(distdir)/$$file; \
	  done)
	-(if test -f guide/index.html; then dir=.; \
	  else dir="${srcdir}"; fi; \
	  cp $${dir}/help/*.html $(distdir)/gnucash; \
	  cp $${dir}/help/figures/*.png $(distdir)/gnucash/figures; \
	  cp $${dir}/help/*.css $(distdir)/gnucash; \
	  cp $${dir}/help/stylesheet-images/*.gif $(distdir)/gnucash/stylesheet-images) \
	  cp $${dir}/guide/*.html $(distdir)/gnucash; \
	  cp $${dir}/guide/figures/*.png $(distdir)/gnucash/figures; \
	  cp $${dir}/guide/*.css $(distdir)/gnucash; \
	  cp $${dir}/guide/stylesheet-images/*.gif $(distdir)/gnucash/stylesheet-images)
