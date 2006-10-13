gncscmmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash/report
gncscmmod_DATA = \
   business-reports.scm \
   aging.scm \
   easy-invoice.scm \
   fancy-invoice.scm \
   payables.scm \
   receivables.scm \
   invoice.scm \
   owner-report.scm

if GNUCASH_SEPARATE_BUILDDIR
SCM_FILE_LINKS = ${gncscmmod_DATA}
endif

.scm-links:
	rm -f gnucash report
	$(LN_S) -f . gnucash
	$(LN_S) -f ${srcdir} report 
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	touch .scm-links

noinst_DATA = .scm-links

EXTRA_DIST = \
  ${gncscmmod_DATA}

CLEANFILES = gnucash report .scm-links
DISTCLEANFILES = ${SCM_FILE_LINKS}
