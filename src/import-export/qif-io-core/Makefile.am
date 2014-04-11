SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-qifiocore.la

AM_CFLAGS = -I${top_srcdir}/src/gnc-module ${GUILE_INCS} ${GLIB_CFLAGS}

libgncmod_qifiocore_la_SOURCES = gncmod-qifiocore.c

noinst_DATA = .scm-links

gncscmdir = ${GNC_SHAREDIR}/scm
gncscm_DATA = \
	qif-acct-table.scm \
	qif-bank-xtn-import.scm \
	qif-file.scm \
	qif-format-check.scm \
	qif-invst-xtn-import.scm \
	qif-objects.scm \
	qif-parse.scm \
	qif-record-xform.scm 

if GNUCASH_SEPARATE_BUILDDIR
SCM_FILE_LINKS = \
  ${gncscm_DATA}
endif

.scm-links:
	rm -f gnucash import-export 
	$(LN_S) -f . gnucash
	$(LN_S) -f . import-export
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	touch .scm-links

gncscmmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash/import-export/
gncscmmod_DATA = qif-io-core.scm

CLEANFILES = gnucash import-export .scm-links
DISTCLEANFILES = ${SCM_FILE_LINKS}
