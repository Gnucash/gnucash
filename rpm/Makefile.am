
EXTRA_DIST = \
  .cvsignore \
  gnucash.spec.in \
  gnucash-sql.spec.in

all-local: gnucash.spec gnucash-sql.spec

## We borrow guile's convention and use @-...-@ as the substitution
## brackets here, instead of the usual @...@.  This prevents autoconf
## from substituting the values directly into the left-hand sides of
## the sed substitutions.  *sigh*
gnucash.spec: gnucash.spec.in Makefile
	rm -f $@.tmp
	sed < $@.in > $@.tmp \
            -e 's:@-VERSION-@:${VERSION}:'
	mv $@.tmp $@

gnucash-sql.spec: gnucash-sql.spec.in Makefile
	rm -f $@.tmp
	sed < $@.in > $@.tmp \
            -e 's:@-VERSION-@:${VERSION}:'
	mv $@.tmp $@

CLEANFILES = gnucash.spec gnucash-sql.spec
