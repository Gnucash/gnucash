
bin_SCRIPTS = gnc-prices
pkgdata_SCRIPTS=price-quote-helper

perllibdir = ${GNC_LIBDIR}/perl

perlsharedir = ${GNC_SHAREDIR}/perl

EXTRA_DIST = \
  .cvsignore \
  Quote_example.pl \
  gnc-prices.in \
  price-quote-helper.in \
  value_portfolio

## We borrow guile's convention and use @-...-@ as the substitution
## brackets here, instead of the usual @...@.  This prevents autoconf
## from substituting the values directly into the left-hand sides of
## the sed substitutions.
gnc-prices: gnc-prices.in
	rm -f $@.tmp
	sed < $@.in > $@.tmp \
            -e 's:@-PERL-@:${PERL}:g' \
            -e 's:@-GNC_PERLSHAREDIR-@:${perlsharedir}:g' \
            -e 's:@-GNC_PERLLIBDIR-@:${perllibdir}:g'
	chmod +x $@.tmp
	mv $@.tmp $@

CLEANFILES += gnc-prices

price-quote-helper: price-quote-helper.in
	rm -f $@.tmp
	sed < $@.in > $@.tmp \
            -e 's:@-PERL-@:${PERL}:g' \
            -e 's:@-PERLINCL-@:${PERLINCL}:g'
	chmod +x $@.tmp
	mv $@.tmp $@

CLEANFILES += price-quote-helper
