
SUBDIRS = . doc lib src intl-scm packaging po accounts checks

docdir = ${GNC_DOC_INSTALL_DIR}

GNC_CTAGS_FILE = @GNC_CTAGS_FILE@
GNC_ETAGS_FILE = @GNC_ETAGS_FILE@

noinst_DATA = make-gnucash-patch make-gnucash-potfiles

dist_doc_DATA = \
  AUTHORS \
  COPYING \
  ChangeLog \
  ChangeLog.2007 \
  ChangeLog.2006 \
  ChangeLog.2005 \
  ChangeLog.2004 \
  ChangeLog.2003 \
  DOCUMENTERS \
  HACKING \
  INSTALL \
  LICENSE \
  NEWS \
  README \
  README.patches \
  README.dependencies

# All the other files that go in the distribution tarfile that aren't
# included some other way...

# Most directories handle their own EXTRA_DIST setting, but for some,
# it's not worth the trouble and for others, there's no Makefile.am
# (i.e. ./intl and ./po -- these handle their own Makefile.in and
# Makefile.in.in files...), so we list their files here.

# Don't list any directories or you'll get *everything*, including the
# CVS dirs.

EXTRA_DIST = \
  gnucash.lsm \
  make-gnucash-patch.in \
  make-gnucash-potfiles.in \
  macros/aqbanking.m4 \
  macros/as-scrub-include.m4 \
  macros/binreloc.m4 \
  macros/compiler-flags.m4 \
  macros/ac_pkg_swig.m4 \
  macros/gnome-guile-checks.m4 \
  macros/legacy_macros.m4 \
  po/gnucash.pot \
  po/POTFILES.in \
  po/POTFILES.skip \
  po/POTFILES.ignore \
  po/README \
  po/glossary/da.po \
  po/glossary/de.po \
  po/glossary/el.po \
  po/glossary/es.po \
  po/glossary/es_NI-policy.txt \
  po/glossary/fr.po \
  po/glossary/gnc-glossary.txt \
  po/glossary/hu.po \
  po/glossary/it.po \
  po/glossary/nl.po \
  po/glossary/nb.po \
  po/glossary/pl.po \
  po/glossary/pt.po \
  po/glossary/pt_BR.po \
  po/glossary/rw.po \
  po/glossary/sk.po \
  po/glossary/sv.po \
  po/glossary/txt-to-pot.sh \
  po/glossary/vi.po \
  po/glossary/zh_TW.po \
  intltool-extract.in \
  intltool-merge.in \
  intltool-update.in \
  util/gnc-svnversion

## We borrow guile's convention and use @-...-@ as the substitution
## brackets here, instead of the usual @...@.  This prevents autoconf
## from substituting the values directly into the left-hand sides of
## the sed substitutions.
make-gnucash-patch: make-gnucash-patch.in Makefile
	rm -f $@.tmp
	sed < $< > $@.tmp \
            -e 's:@-PERL-@:${PERL}:g'
	chmod +x $@.tmp
	mv $@.tmp $@

make-gnucash-potfiles: make-gnucash-potfiles.in Makefile
	rm -f $@.tmp
	sed < $< > $@.tmp \
            -e 's:@-SRCDIR-@:${srcdir}:g' \
            -e 's:@-PERL-@:${PERL}:g'
	chmod +x $@.tmp
	mv $@.tmp $@

DISTCLEANFILES = \
  intltool-extract intltool-update intltool-merge\
  cscope.files cscope.out etags.files make-gnucash-patch \
  make-gnucash-potfiles po/.intltool-merge-cache

cscope.files:
	find src lib -name '*.[ch]' > cscope.files

cscope.out: cscope.files
	cscope -b


# make sure etags.files is up to date.
# and use the contents of etags.files to re-make TAGS if needed.
etags:
if GNC_ETAGS_FILE
	${MAKE} -f Makefile.TAGS etags.files
	${MAKE} -f Makefile.TAGS TAGS
else
	@echo "You must ./configure with --enable-etags to use etags."
endif


# make sure etags.files is up to date.
# and use the contents of etags.files to re-make tags if needed.
ctags:
if GNC_CTAGS_FILE
	${MAKE} -f Makefile.TAGS etags.files
	${MAKE} -f Makefile.TAGS tags
else
	@echo "You must ./configure with --enable-ctags to use ctags."
endif

.PHONY: etags ctags

ACLOCAL_AMFLAGS = -I macros -I m4

.PHONY: doc
doc:
	$(MAKE) -C src/doc doc

distcleancheck_listfiles = \
  find -type f -exec sh -c 'test -f ${srcdir}/{} || echo {}' ';'

#dist-hook: po/POTFILES.in

distcheck-hook:
	@e=''; \
	for X in `grep -v \# ${distdir}/po/POTFILES.in` ; do \
		if [ ! -f ${distdir}/$$X ] ; then \
			echo $$X " is in POTFILES.in but not in the dist."; \
			e='yes'; \
		fi; \
	done; \
	for X in `grep -v \# ${distdir}/po/POTFILES.skip` ; do \
		if [ -f ${distdir}/$$X ] ; then \
			echo $$X " is in POTFILES.skip but also in the dist.";\
			e='yes'; \
		fi; \
	done; \
	if test "$$e" = "yes" ; then exit 1; fi;

distclean-local:

libtool: $(LIBTOOL_DEPS)
	$(SHELL) ./config.status --recheck

.PHONY: pot
pot: Makefile po/POTFILES.in
	rm -f intl-scm/guile-strings.c
	${MAKE} -C intl-scm
	rm -f po/$(PACKAGE).pot
	${MAKE} -C po $(PACKAGE).pot


$(srcdir)/po/POTFILES.in: make-gnucash-potfiles .potfiles
	./make-gnucash-potfiles > $(srcdir)/po/POTFILES.in

# Creation rules so that po/gnucash.pot can always be created for
# make dist.
po/gnucash.pot: po/POTFILES.in intl-scm/guile-strings.c
	${MAKE} -C po gnucash.pot
intl-scm/guile-strings.c:
	${MAKE} -C intl-scm guile-strings.c

.potfiles:

SVN = svn
XSLTPROC = xsltproc

if BUILDING_FROM_SVN
# This rule will generate a ChangeLog of gnucash/trunk commits, plus
# all additional branches that are added in
# ./macros/svn2cl.xsl. (FIXME: Is the dependency on NEWS really a good
# idea? Before a release, NEWS will get updated because of the version
# change, so this should fit just fine.)
ChangeLog: NEWS
	$(SVN) log -v --xml -r HEAD:16778 http://svn.gnucash.org/repo/gnucash | \
	  $(XSLTPROC) --stringparam strip-prefix "gnucash/branches/2.2" \
	  --stringparam include-rev "yes" $(srcdir)/macros/svn2cl.xsl - > $@
endif
