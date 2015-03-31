
SUBDIRS = . doc lib src packaging po accounts checks

GNC_CTAGS_FILE = @GNC_CTAGS_FILE@
GNC_ETAGS_FILE = @GNC_ETAGS_FILE@

noinst_DATA = make-gnucash-potfiles

noinst_PROGRAMS    = gnc-guile
gnc_guile_SOURCES = util/guile.c
gnc_guile_CFLAGS  = ${GUILE_CFLAGS}
gnc_guile_LDADD   = ${GUILE_LIBS}

dist_doc_DATA = \
  AUTHORS \
  COPYING \
  ChangeLog \
  ChangeLog.2014 \
  ChangeLog.2013 \
  ChangeLog.2012 \
  ChangeLog.2011 \
  ChangeLog.2010 \
  ChangeLog.2009 \
  ChangeLog.2008 \
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
  $(SWIG_DIST_FAIL) \
  make-gnucash-potfiles.in \
  macros/as-scrub-include.m4 \
  macros/binreloc.m4 \
  macros/compiler-flags.m4 \
  macros/ax_pkg_swig.m4 \
  macros/ax_python_devel.m4 \
  macros/ax_swig_python.m4 \
  macros/legacy_macros.m4 \
  po/gnucash.pot \
  po/POTFILES.in \
  po/POTFILES.skip \
  po/POTFILES.ignore \
  po/README \
  po/glossary/bg.po \
  po/glossary/ca.po \
  po/glossary/da.po \
  po/glossary/de.po \
  po/glossary/el.po \
  po/glossary/es.po \
  po/glossary/es_NI-policy.txt \
  po/glossary/fr.po \
  po/glossary/gnc-glossary.txt \
  po/glossary/he.po \
  po/glossary/hu.po \
  po/glossary/it.po \
  po/glossary/nb.po \
  po/glossary/nl.po \
  po/glossary/pl.po \
  po/glossary/pt.po \
  po/glossary/pt_BR.po \
  po/glossary/ru.po \
  po/glossary/rw.po \
  po/glossary/sk.po \
  po/glossary/sv.po \
  po/glossary/txt-to-pot.sh \
  po/glossary/vi.po \
  po/glossary/zh_CN.po \
  po/glossary/zh_TW.po \
  intltool-extract.in \
  intltool-merge.in \
  intltool-update.in \
  util/gnc-vcs-info

## We borrow guile's convention and use @-...-@ as the substitution
## brackets here, instead of the usual @...@.  This prevents autoconf
## from substituting the values directly into the left-hand sides of
## the sed substitutions.
make-gnucash-potfiles: make-gnucash-potfiles.in Makefile
	rm -f $@.tmp
	sed < $< > $@.tmp \
            -e 's:@-SRCDIR-@:${srcdir}:g' \
            -e 's:@-PERL-@:${PERL}:g'
	chmod +x $@.tmp
	mv $@.tmp $@

DISTCLEANFILES = \
  intltool-extract intltool-update intltool-merge\
  cscope.files cscope.out etags.files \
  make-gnucash-potfiles po/.intltool-merge-cache

cscope.files:
	find src lib -name '*.[ch]' > cscope.files
if GNUCASH_SEPARATE_BUILDDIR
	find ${srcdir}/src ${srcdir}/lib -name '*.[ch]' >> cscope.files
endif

${srcdir}/cscope.out: cscope.files
	cscope -b -f ${srcdir}/cscope.out


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

ACLOCAL_AMFLAGS = -I macros

.PHONY: doc
doc:
	$(MAKE) -C src/doc doc

distcleancheck_listfiles = \
  find -type f -exec sh -c 'test -f ${srcdir}/{} || echo {}' ';'
distuninstallcheck_listfiles = \
  find . -type f -print | grep -v ./share/icons/hicolor/icon-theme.cache | grep -v ./share/info/dir

#dist-hook: po/POTFILES.in

# Make "make dist" fail if we have the wrong version of swig
if SWIG_DIST_FAIL
SWIG_DIST_FAIL = swig-dist-fail
swig-dist-fail:
	@echo "You cannot build the dist with your version of swig"
	@exit 1
endif

distcheck-hook:
	@e=''; \
	for X in `grep -v \# ${distdir}/po/POTFILES.in | sed 's/\[type:.*\]//'` ; do \
		if [ ! -f ${distdir}/$$X ] ; then \
			echo $$X " is in POTFILES.in but not in the dist."; \
			e='yes'; \
		fi; \
	done; \
	for X in `grep -v '\(^\#\|.scm\$\)' ${distdir}/po/POTFILES.skip` ; do \
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
	rm -f po/$(PACKAGE).pot
	${MAKE} -C po $(PACKAGE).pot


$(srcdir)/po/POTFILES.in: make-gnucash-potfiles .potfiles
	if test -w $(srcdir)/po/POTFILES.in ; then ./make-gnucash-potfiles > $(srcdir)/po/POTFILES.in ; fi

# Creation rules so that po/gnucash.pot can always be created for
# make dist.
po/gnucash.pot: po/POTFILES.in
	${MAKE} -C po gnucash.pot

.potfiles:

SVN = svn

if BUILDING_FROM_VCS
# This rule will generate a ChangeLog of gnucash/trunk commits, plus
# all additional branches that are added in
# ./macros/svn2cl.xsl. (FIXME: Is the dependency on NEWS really a good
# idea? Before a release, NEWS will get updated because of the version
# change, so this should fit just fine.)
ChangeLog: NEWS
	cd $(abs_srcdir) ; \
	if test x$(VCS_TYPE) = xsvn ; then \
	  $(SVN) log -v --xml -r HEAD:'{2015-01-01}' http://svn.gnucash.org/repo/gnucash | \
	    $(XSLTPROC) --stringparam strip-prefix "gnucash/trunk" \
	    --stringparam include-rev "yes" $(abs_srcdir)/macros/svn2cl.xsl - > $(abs_builddir)/$@ ; \
	else \
	  if test x$(VCS_TYPE) = xgit ; then \
	    "$(GIT_CMD)" log --format="%ad %aN %n%n%x09* %s%d%n" --date=short --since=2013-01-01 > $(abs_builddir)/$@ ; \
	  else \
	    touch $(abs_builddir)/$@ ; \
	    echo "Note: not in svn or git. ChangeLog not regenerated." ; \
	  fi ; \
	fi ; \
	cd $(abs_builddir)
endif

ASTYLE = /usr/bin/astyle
.PHONY: indent
indent:
	$(ASTYLE) --indent=spaces=4 --brackets=break --suffix=none `find ${srcdir}/src -name '*.[hc]'`
# Use the following line if you've got astyle-1.24, but don't use
# --pad=oper with astyle 1.22 as it will reformat e.g. "return +1;" in
# a very ugly way.
#	$(ASTYLE) --indent=spaces=4 --brackets=break --pad-oper -pad-header --suffix=none `find ${srcdir}/src -name '*.[hc]'`
	@echo "### GnuCash development hint: The above command might have re-indented much more files than what you intended. Please commit only those which you really want to have changed, and revert the changes in the others so that other devevelopers do not have unnecessary merge conflicts. Thanks! ###"

