
SUBDIRS = intl po src rpm doc

docdir = ${GNC_DOC_INSTALL_DIR}

noinst_DATA = make-gnucash-patch

doc_DATA = \
  AUTHORS \
  COPYING \
  ChangeLog \
  INSTALL \
  NEWS \
  README

## We borrow guile's convention and use @-...-@ as the substitution
## brackets here, instead of the usual @...@.  This prevents autoconf
## from substituting the values directly into the left-hand sides of
## the sed substitutions.
make-gnucash-patch: make-gnucash-patch.in
	rm -f $@.tmp
	sed < $@.in > $@.tmp \
            -e 's:@-PERL-@:${PERL}:g'
	chmod +x $@.tmp
	mv $@.tmp $@

CLEANFILES += make-gnucash-patch


