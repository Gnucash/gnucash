.scm-links:
	rm -f gnucash report
	ln -sf . gnucash
	ln -sf ${srcdir} report 
	touch .scm-links

noinst_DATA = .scm-links

gncscmmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash/report
gncscmmod_DATA = \
   business-reports.scm \
   invoice.scm

EXTRA_DIST = \
  .cvsignore \
  ${gncscmmod_DATA}

CLEANFILES = gnucash report .scm-links
