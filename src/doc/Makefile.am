
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
  doxygen.cfg

docdir = ${GNC_DOC_INSTALL_DIR}

all:

doc: doxygen.cfg
	echo "doc: " && pwd && 	echo "distdir: " && echo $(distdir)
	rm -rf html/ refman.pdf
	$(DOXYGEN) doxygen.cfg
#	$(MAKE) -C latex/ 
#	mv latex/refman.pdf ./refman.pdf

dist-hook:
	echo "dist-hook: " && pwd
	cd $(srcdir) && pwd && rm -rf html refman.pdf && $(DOXYGEN) doxygen.cfg
	cp -rp $(srcdir)/html ${distdir}

clean-local:
	echo "clean-local: " && pwd
	rm -rf latex/
	rm -f *~
	rm -f doxygen.log

maintainer-clean-local: clean-local
	echo "maintainer-clean-local: " && pwd
	rm -rf html refman.pdf

install-data-hook:
	mkdir -p $(srcdir)/html #Workaround to allow libofx-cvs user to install without doc.
	cp -rp $(srcdir)/html ${docdir}

uninstall-hook:
	rm -rf ${docdir}/html
