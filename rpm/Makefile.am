
EXTRA_DIST = \
  .cvsignore \
  gnucash.spec.in

all-local: gnucash.spec

## We borrow guile's convention and use @-...-@ as the substitution
## brackets here, instead of the usual @...@.  This prevents autoconf
## from substituting the values directly into the left-hand sides of
## the sed substitutions.  *sigh*
gnucash.spec: gnucash.spec.in Makefile
	rm -f $@.tmp
	sed < $@.in > $@.tmp \
            -e 's:@-VERSION-@:${VERSION}:'
	mv $@.tmp $@

CLEANFILES = gnucash.spec
