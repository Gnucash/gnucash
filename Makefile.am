
SUBDIRS = macros debian doc intl lib src po rpm accounts

docdir = ${GNC_DOC_INSTALL_DIR}

noinst_DATA = make-gnucash-patch

doc_DATA = \
  AUTHORS \
  COPYING \
  ChangeLog \
  HACKING \
  INSTALL \
  NEWS \
  README

# All the other files that go in the distribution tarfile that aren't
# included some other way...

# Most directories handle their own EXTRA_DIST setting, but for some,
# it's not worth the trouble (i.e ./old), and for others, there's no
# Makefile.am (i.e. ./intl and ./po -- these handle their own
# Makefile.in and Makefile.in.in files...), so we list their files
# here.

# Don't list any directories or you'll get *everything*, including the
# CVS dirs.

EXTRA_DIST = \
  .cvsignore \
  HACKING \
  gnucash.lsm \
  intl/.cvsignore \
  make-gnucash-patch.in \
  old/country.sql \
  old/currencynames \
  old/Gnucash.app-defaults \
  old/gnome/graph/Makefile \
  old/gnome/graph/colors.c \
  old/gnome/graph/design.txt \
  old/gnome/graph/plot-test.c \
  old/perl-reports/report-baln.phtml \
  old/perl-reports/report-folio.phtml \
  old/perl-reports/report-formtest.html \
  old/perl-reports/report-main.html \
  old/perl-reports/report-menu.html \
  old/perl-reports/report-null.html \
  old/perl-reports/report-pnl.phtml \
  old/xacc-shell.c \
  po/.cvsignore \
  po/extract-macros.perl.in \
  po/guile_strings.txt

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

CLEANFILES +=  cscope.files etags.files make-gnucash-patch

cscope.files:
	find . -name '*.[ch]' > cscope.files

cscope.out: cscope.files
	cscope -b

TAGS: etags.files
	etags -L - < etags.files

etags.files:
	find . -name '*.[ch]' -o -name '*.scm' > etags.files
