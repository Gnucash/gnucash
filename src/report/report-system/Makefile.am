SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-report-system.la

libgncmod_report_system_la_SOURCES = \
  swig-report-system.c \
  gncmod-report-system.c \
  gnc-report.c

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  gnc-report.h

libgncmod_report_system_la_LIBADD = \
  ${top_builddir}/src/gnc-module/libgncmodule.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${GUILE_LIBS} \
  ${GLIB_LIBS}

swig-report-system.c: report-system.i ${gncinclude_HEADERS}
	swig -guile $(SWIG_ARGS) -Linkage module -o $@ $<

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/app-utils \
  ${GLIB_CFLAGS} ${GUILE_INCS} \
  ${G_WRAP_COMPILE_ARGS}

gncscmdir = ${GNC_SHAREDIR}/scm
gncscm_DATA = \
     commodity-utilities.scm  \
     html-acct-table.scm \
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

gncmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash/report-system

gncscmmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash/report/
gncscmmod_DATA = report-system.scm

if GNUCASH_SEPARATE_BUILDDIR
#For executing test cases
# for running
SCM_FILE_LINKS = \
  ${gncscmmod_DATA} \
  ${gncscm_DATA}
endif

.scm-links:
	rm -f gnucash report
	$(LN_S) -f . gnucash
	$(LN_S) -f . report 
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	touch .scm-links

noinst_DATA = .scm-links

EXTRA_DIST = ${gncscmmod_DATA} ${gncscm_DATA}

BUILT_SOURCES = swig-report-system.c
CLEANFILES = $(BUILT_SOURCES) gnucash report .scm-links ${SCM_FILE_LINKS}
