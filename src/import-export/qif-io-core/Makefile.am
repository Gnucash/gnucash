SUBDIRS=. test

lib_LTLIBRARIES=libgncmod-qifiocore.la
CFLAGS=@CFLAGS@ ${GLIB_CFLAGS}

INCLUDES = -I${top_srcdir}/src/gnc-module ${GUILE_INCS}

libgncmod_qifiocore_la_SOURCES=gnc-qifiocore.c
libgncmod_qifiocore_la_LDFLAGS=-module

noinst_DATA=.scm-links

.scm-links:
	rm -f gnucash import-export 
	ln -sf . gnucash
	ln -sf . import-export
	touch .scm-links

gncscmmoddir=${GNC_SHAREDIR}/guile-modules/gnucash/import-export/
gncscmmod_DATA=qif-io-core.scm

gncscmdir=${GNC_SHAREDIR}/scm
gncscm_DATA=\
	qif-acct-table.scm \
	qif-bank-xtn-import.scm \
	qif-file.scm \
	qif-format-check.scm \
	qif-invst-xtn-import.scm \
	qif-objects.scm \
	qif-parse.scm \
	qif-record-xform.scm 

CLEANFILES += gnucash import-export .scm-links
