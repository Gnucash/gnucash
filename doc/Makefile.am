
docdir = ${GNC_DOC_INSTALL_DIR}

SUBDIRS = examples

doc_DATA = \
  README.francais \
  README.german \
  guile-hackers.txt \
  projects.html 

man_MANS = \
  gnc-prices.1 \
  gnucash.1

EXTRA_DIST = \
  .cvsignore \
  ${doc_DATA} \
  RAW-NOTES \
  README.build-system \
  README.francais \
  README.german \
  README.HBCI \
  README.translator.txt \
  build-aix.txt \
  build-solaris.txt \
  build-suse.txt \
  gnome-hackers.txt \
  gnc-prices.1.in \
  gnucash.1.in

## We borrow guile's convention and use @-...-@ as the substitution
## brackets here, instead of the usual @...@.  This prevents autoconf
## from substituting the values directly into the left-hand sides of
## the sed substitutions.
gnc-prices.1: gnc-prices.1.in
	rm -f $@.tmp
	sed < $< > $@.tmp \
            -e 's:@-VERSION-@:${VERSION}:g' \
            -e 's:@-DATE-@:$(shell date +'%B %Y'):g'
	chmod +x $@.tmp
	mv $@.tmp $@

gnucash.1: gnucash.1.in
	rm -f $@.tmp
	sed < $< > $@.tmp \
            -e 's:@-VERSION-@:${VERSION}:g' \
            -e 's:@-DATE-@:$(shell date +'%B %Y'):g'
	chmod +x $@.tmp
	mv $@.tmp $@

DISTCLEANFILES = gnc-prices.1 gnucash.1
