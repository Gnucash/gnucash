gncscmdir = ${GNC_SCM_INSTALL_DIR}/gnumeric
gncscm_DATA = \
  gnumeric-utilities.scm \
  table-utils.scm

if GNUCASH_SEPARATE_BUILDDIR
SCM_FILE_LINKS = \
  ${gncscm_DATA}
endif

.scm-links:
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
if ! OS_WIN32
# Windows knows no "ln -s" but uses "cp": must copy every time (see bug #566567).
	touch .scm-links
endif

if GNC_HAVE_GUILE_2
%.go : %.scm .scm-links
	$(GUILD) compile \
	-o $@ $<

gncscmcachedir = ${GNC_SCM_INSTALL_DIR}/ccache/@GUILE_EFFECTIVE_VERSION@/gnumeric
gncscmcache_DATA = $(gncscm_DATA:.scm=.go)
endif

noinst_DATA = .scm-links

EXTRA_DIST = \
  ${gncscm_DATA}

CLEANFILES = .scm-links
DISTCLEANFILES = ${SCM_FILE_LINKS}
