bin_SCRIPTS = gnc-fq-check gnc-fq-dump gnc-fq-helper gnc-fq-update

EXTRA_DIST = \
  Quote_example.pl \
  gnc-fq-check.in \
  gnc-fq-dump \
  gnc-fq-helper.in \
  gnc-fq-update.in \
  CMakeLists.txt

## We borrow guile's convention and use @-...-@ as the substitution
## brackets here, instead of the usual @...@.  This prevents autoconf
## from substituting the values directly into the left-hand sides of
## the sed substitutions.
gnc-fq-helper: gnc-fq-helper.in Makefile
	rm -f $@.tmp
	sed < $< > $@.tmp \
            -e 's:@-PERL-@:${PERL}:g' \
            -e 's:@-PERLINCL-@:${PERLINCL}:g'
	chmod +x $@.tmp
	mv $@.tmp $@

gnc-fq-check: gnc-fq-check.in Makefile
	rm -f $@.tmp
	sed < $< > $@.tmp \
            -e 's:@-PERL-@:${PERL}:g' \
            -e 's:@-PERLINCL-@:${PERLINCL}:g'
	chmod +x $@.tmp
	mv $@.tmp $@

gnc-fq-update: gnc-fq-update.in Makefile
	rm -f $@.tmp
	sed < $< > $@.tmp \
            -e 's:@-PERL-@:${PERL}:g' \
            -e 's:@-PERLINCL-@:${PERLINCL}:g'
	chmod +x $@.tmp
	mv $@.tmp $@

CLEANFILES = gnc-fq-helper gnc-fq-check gnc-fq-update
