SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-report-system.la

libgncmod_report_system_la_SOURCES = \
  gncmod-report-system.c \
  gnc-report.c

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  gnc-report.h

libgncmod_report_system_la_LDFLAGS = -module
libgncmod_report_system_la_LIBADD = \
  -L../../gnc-module -L../../gnc-module/.libs -lgncmodule \
  ${GLIB_LIBS} \
  ${GUILE_LIBS} 


AM_CFLAGS = -I${top_srcdir}/src/gnc-module ${GLIB_CFLAGS} ${GUILE_INCS}

.scm-links:
	rm -f gnucash report
	ln -sf . gnucash
	ln -sf . report 
	touch .scm-links

gncscmmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash/report/
gncscmmod_DATA = report-system.scm

noinst_DATA = .scm-links

gncscmdir = ${GNC_SHAREDIR}/scm
gncscm_DATA = \
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

EXTRA_DIST = ${gncscmmod_DATA} ${gncscm_DATA}

CLEANFILES = gnucash report .scm-links
