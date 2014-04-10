
gncsharedir = ${GNC_SHAREDIR}

bin_SCRIPTS = dump-finance-quote gnc-prices update-finance-quote
gncshare_SCRIPTS = finance-quote-helper finance-quote-check

EXTRA_DIST = \
  .cvsignore \
  Quote_example.pl \
  finance-quote-check.in \
  finance-quote-helper.in \
  gnc-prices \
  update-finance-quote.in \
  value_portfolio \
  dump-finance-quote

## We borrow guile's convention and use @-...-@ as the substitution
## brackets here, instead of the usual @...@.  This prevents autoconf
## from substituting the values directly into the left-hand sides of
## the sed substitutions.
finance-quote-helper: finance-quote-helper.in
	rm -f $@.tmp
	sed < $< > $@.tmp \
            -e 's:@-PERL-@:${PERL}:g' \
            -e 's:@-PERLINCL-@:${PERLINCL}:g'
	chmod +x $@.tmp
	mv $@.tmp $@

finance-quote-check: finance-quote-check.in
	rm -f $@.tmp
	sed < $< > $@.tmp \
            -e 's:@-PERL-@:${PERL}:g' \
            -e 's:@-PERLINCL-@:${PERLINCL}:g'
	chmod +x $@.tmp
	mv $@.tmp $@

update-finance-quote: update-finance-quote.in
	rm -f $@.tmp
	sed < $< > $@.tmp \
            -e 's:@-PERL-@:${PERL}:g' \
            -e 's:@-PERLINCL-@:${PERLINCL}:g'
	chmod +x $@.tmp
	mv $@.tmp $@

CLEANFILES = finance-quote-helper finance-quote-check update-finance-quote
