
SUBDIRS = macros debian intl doc-tools doc lib src intl-scm po rpm accounts

docdir = ${GNC_DOC_INSTALL_DIR}

noinst_DATA = make-gnucash-patch make-gnucash-potfiles @GNC_TAGS_FILE@

doc_DATA = \
  AUTHORS \
  COPYING \
  ChangeLog \
  ChangeLog.1 \
  HACKING \
  INSTALL \
  NEWS \
  README \
  README.patches

m4datadir = $(datadir)/aclocal
m4data_DATA = gnucash.m4

# All the other files that go in the distribution tarfile that aren't
# included some other way...

# Most directories handle their own EXTRA_DIST setting, but for some,
# it's not worth the trouble and for others, there's no Makefile.am
# (i.e. ./intl and ./po -- these handle their own Makefile.in and
# Makefile.in.in files...), so we list their files here.

# Don't list any directories or you'll get *everything*, including the
# CVS dirs.

EXTRA_DIST = \
  .cvsignore \
  ChangeLog.1 \
  HACKING \
  README.patches \
  gnucash.lsm \
  gnucash.m4 \
  gnucash-config.in \
  make-gnucash-patch.in \
  make-gnucash-potfiles.in \
  po/.cvsignore \
  po/README \
  po/glossary/da.po \
  po/glossary/de.po \
  po/glossary/es.po \
  po/glossary/es_NI-policy.txt \
  po/glossary/gnc-glossary.txt \
  po/glossary/txt-to-pot.sh \
  intltool-extract.in \
  intltool-merge.in \
  intltool-update.in

bin_SCRIPTS = gnucash-config

CLEANFILES = gnucash-config

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

make-gnucash-potfiles: make-gnucash-potfiles.in
	rm -f $@.tmp
	sed < $@.in > $@.tmp \
            -e 's:@-PERL-@:${PERL}:g'
	chmod +x $@.tmp
	mv $@.tmp $@

DISTCLEANFILES = \
  cscope.files cscope.out etags.files make-gnucash-patch

cscope.files:
	find . -name '*.[ch]' > cscope.files

cscope.out: cscope.files
	cscope -b

if GNC_TAGS_FILE

TAGS:
        # make sure etags.files is up to date.
	${MAKE} -f Makefile.TAGS etags.files
        # now use the contents of etags.files to re-make TAGS if needed.
	${MAKE} -f Makefile.TAGS TAGS

else

TAGS:
	@echo "You must ./configure with --enable-etags to use TAGS."

endif

.PHONY: TAGS
