SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-qifiocore.la

AM_CPPFLAGS = -I${top_srcdir}/src/gnc-module ${GUILE_INCS} ${GLIB_CFLAGS}

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
	$(RM) -rf gnucash
	mkdir -p  gnucash
	mkdir -p  gnucash/import-export
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	( cd gnucash/import-export; for A in $(gncscmmod_DATA) ; do $(LN_S) -f ../../$$A . ; done )
if ! OS_WIN32
# Windows knows no "ln -s" but uses "cp": must copy every time (see bug #566567).
	touch .scm-links
endif

gncscmmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash/import-export/
gncscmmod_DATA = qif-io-core.scm

clean-local:
	$(RM) -rf qif-import

CLEANFILES = .scm-links
DISTCLEANFILES = ${SCM_FILE_LINKS}

INCLUDES = -DG_LOG_DOMAIN=\"gnc.import.qif.core\"
