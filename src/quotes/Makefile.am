
gncsharedir = ${GNC_SHAREDIR}

bin_SCRIPTS = gnc-prices
gncshare_SCRIPTS=finance-quote-helper

EXTRA_DIST = \
  .cvsignore \
  Quote_example.pl \
  finance-quote-helper.in \
  gnc-prices \
  value_portfolio

## We borrow guile's convention and use @-...-@ as the substitution
## brackets here, instead of the usual @...@.  This prevents autoconf
## from substituting the values directly into the left-hand sides of
## the sed substitutions.
finance-quote-helper: finance-quote-helper.in
	rm -f $@.tmp
	sed < $@.in > $@.tmp \
            -e 's:@-PERL-@:${PERL}:g' \
            -e 's:@-PERLINCL-@:${PERLINCL}:g'
	chmod +x $@.tmp
	mv $@.tmp $@

CLEANFILES += finance-quote-helper
