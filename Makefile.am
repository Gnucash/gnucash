
SUBDIRS = intl  m4  macros doc lib src intl-scm po rpm accounts

docdir = ${GNC_DOC_INSTALL_DIR}

GNC_CTAGS_FILE = @GNC_CTAGS_FILE@
GNC_ETAGS_FILE = @GNC_ETAGS_FILE@

noinst_DATA = make-gnucash-patch make-gnucash-potfiles

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

EXTRA_DIST = config.rpath  config.rpath  \
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
	sed < $< > $@.tmp \
            -e 's:@-PERL-@:${PERL}:g'
	chmod +x $@.tmp
	mv $@.tmp $@

make-gnucash-potfiles: make-gnucash-potfiles.in
	rm -f $@.tmp
	sed < $< > $@.tmp \
            -e 's:@-PERL-@:${PERL}:g'
	chmod +x $@.tmp
	mv $@.tmp $@

DISTCLEANFILES = \
  intltool-extract intltool-update intltool-merge\
  cscope.files cscope.out etags.files make-gnucash-patch \
  make-gnucash-potfiles po/.intltool-merge-cache

cscope.files:
	find . -name '*.[ch]' > cscope.files

cscope.out: cscope.files
	cscope -b

if GNC_ETAGS_FILE

etags:
        # make sure etags.files is up to date.
	${MAKE} -f Makefile.TAGS etags.files
        # now use the contents of etags.files to re-make TAGS if needed.
	${MAKE} -f Makefile.TAGS etags

else

etags:
	@echo "You must ./configure with --enable-etags to use ctags."

endif

if GNC_CTAGS_FILE

ctags:
        # make sure etags.files is up to date.
	${MAKE} -f Makefile.TAGS etags.files
        # now use the contents of etags.files to re-make TAGS if needed.
	${MAKE} -f Makefile.TAGS ctags

else

ctags:
	@echo "You must ./configure with --enable-ctags to use ctags."

endif

.PHONY: etags ctags

ACLOCAL_AMFLAGS = -I m4

.PHONY: doc
doc:
	$(MAKE) -C src/doc doc

distcleancheck_listfiles = \
  find -type f -exec sh -c 'test -f ${srcdir}/{} || echo {}' ';'

# ack this is stupid
distcheck-hook:
	: > "$(distdir)/=build/distcleancheck1.5-is-stupid"

distclean-local:
	-test -f distcleancheck1.5-is-stupid \
	&& rm -f distcleancheck1.5-is-stupid \
	&& find . -type f -exec sh -c 'test -f ${srcdir}/{} && rm -f {}' ';'

libtool: $(LIBTOOL_DEPS)
	$(SHELL) ./config.status --recheck
