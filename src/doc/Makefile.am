
SUBDIRS = \
  design \
  xml

EXTRA_DIST = \
  backup.txt \
  budget.txt \
  constderv.html \
  finderv.html \
  finutil.html \
  plugin.txt \
  tax.txt \
  doxygen.cfg.in

docdir = ${GNC_DOC_INSTALL_DIR}

all:

doc: doxygen.cfg
	echo "doc: " && pwd && 	echo "distdir: " && echo $(distdir)
	rm -rf html refman.pdf
	$(DOXYGEN) doxygen.cfg
#	$(MAKE) -C latex/ 
#	mv latex/refman.pdf ./refman.pdf

dist-hook:
	echo "dist-hook: " && pwd
	cd $(srcdir) && pwd && rm -rf html refman.pdf && $(DOXYGEN) doxygen.cfg
	cp -rp html ${distdir}

clean-local:
	echo "clean-local: " && pwd
	rm -rf latex/
	rm -f doxygen.log

maintainer-clean-local: clean-local
	echo "maintainer-clean-local: " && pwd
	rm -rf html refman.pdf

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
            -e 's:@-top_srcdir-@:${top_srcdir}:g'
	mv $@.tmp $@

