SUBDIRS=. test

pkglib_LTLIBRARIES=libgncmod-report-system.la

libgncmod_report_system_la_SOURCES=gncmod-report-system.c 
libgncmod_report_system_la_LDFLAGS=-module
libgncmod_report_system_la_LIBADD = \
  -L../../gnc-module -L../../gnc-module/.libs -lgncmodule \
  -L../../app-utils -L../../app-utils/.libs -lgncmod-app-utils \
  ${GLIB_LIBS} ${GUILE_LIBS} 


INCLUDES=-I${top_srcdir}/src/gnc-module ${GLIB_CFLAGS}

.scm-links:
	rm -f gnucash report
	ln -sf . gnucash
	ln -sf . report 
	touch .scm-links

gncscmmoddir=${GNC_SHAREDIR}/guile-modules/gnucash/report/
gncscmmod_DATA=report-system.scm

noinst_DATA=.scm-links

gncscmdir=${GNC_SHAREDIR}/scm
gncscm_DATA=\
     commodity-utilities.scm  \
     html-barchart.scm \
     html-document.scm \
     html-piechart.scm \
     html-scatter.scm \
     html-style-info.scm \
     html-style-sheet.scm \
     html-table.scm \
     html-text.scm \
     html-utilities.scm \
     options-utilities.scm \
     report-utilities.scm \
     report.scm 

CLEANFILES += gnucash report .scm-links


