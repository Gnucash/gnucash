
SUBDIRS = \
  design \
  xml

EXTRA_DIST = \
  README \
  backup.txt \
  budget.txt \
  constderv.html \
  finderv.html \
  finutil.html \
  plugin.txt \
  tax.txt \
  doxygen_main_page.c \
  doxygen.cfg.in \
  TODO-schedxactions \
  TODO-sixtp \
  backend-api.txt \
  backend-errors.txt \
  books.txt \
  currencies.txt \
  gnc-commodity.txt \
  loans.txt \
  lots.txt \
  multicurrency-discussion.txt \
  netlogin.txt \
  query-api.txt \
  guid.txt \
  qif.txt \
  user-prefs-howto.txt


docdir = ${GNC_DOC_INSTALL_DIR}

all:

doc: doxygen.cfg
	echo "doc: " && pwd && 	echo "distdir: " && echo $(distdir)
	rm -rf html refman.pdf
	$(DOXYGEN) doxygen.cfg
#	$(MAKE) -C latex/ 
#	mv latex/refman.pdf ./refman.pdf

dist-hook: doc
	echo "dist-hook: " && pwd
	cp -rp html ${distdir}

distclean-local:
	echo "distclean-local: " && pwd
	rm -f doxygen.cfg doxygen.log

maintainer-clean-local:
	echo "maintainer-clean-local: " && pwd
	rm -rf html latex
	rm -f refman.pdf

install-data-hook:
	$(mkinstalldirs) $(DESTDIR)$(docdir)/html
	mkdir -p html #Workaround to allow libofx-cvs user to install without doc.
#	cp -rp $(srcdir)/html ${docdir}
	cp -rp html $(DESTDIR)$(docdir)/html

uninstall-hook:
	rm -rf ${docdir}/html

## We borrow guile's convention and use @-...-@ as the substitution
## brackets here, instead of the usual @...@.  This prevents autoconf
## from substituting the values directly into the left-hand sides of
## the sed substitutions.
doxygen.cfg: doxygen.cfg.in Makefile
	rm -f $@.tmp
	sed < $< > $@.tmp \
            -e 's:@-top_srcdir-@:${top_srcdir}:g; s:@-VERSION-@:${VERSION}:g'
	mv $@.tmp $@

