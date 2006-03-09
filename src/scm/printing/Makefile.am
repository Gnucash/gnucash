gncscmdir = ${GNC_SCM_INSTALL_DIR}/printing
gncscm_DATA = print-check.scm

gncscmmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash/printing
gncscmmod_DATA = number-to-words.scm

if GNUCASH_SEPARATE_BUILDDIR
SCM_FILE_LINKS = \
  ${gncscmmod_DATA} \
  ${gncscm_DATA}
endif

.scm-links:
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	touch .scm-links

noinst_DATA = .scm-links

EXTRA_DIST = ${gncscmmod_DATA} ${gncscm_DATA}
CLEANFILES = .scm-links
DISTCLEANFILES = ${SCM_FILE_LINKS}
