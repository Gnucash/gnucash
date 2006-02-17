SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-report-system.la libgw-report-system.la

libgncmod_report_system_la_SOURCES = \
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
# for compiling
SCM_FILE_LINKS = gw-report-system-spec.scm
# for running
SCM_FILE_LINKS += \
  ${gncscmmod_DATA} \
  ${gncscm_DATA}
endif

.scm-links:
	rm -f gnucash report g-wrapped
	ln -sf . gnucash
	ln -sf . report 
	ln -sf . g-wrapped
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  ln -sf ${srcdir}/$$X . ; \
	done
endif
	touch .scm-links

libgw_report_system_la_SOURCES = gw-report-system.c
gwmoddir = ${GNC_GWRAP_LIBDIR}
gwmod_DATA = gw-report-system.scm gw-report-system-spec.scm

noinst_HEADERS = gw-report-system.h
noinst_DATA = .scm-links gw-report-system.h

EXTRA_DIST = ${gncscmmod_DATA} ${gncscm_DATA} ${gwmod_DATA}

gw-report-system.scm gw-report-system.c gw-report-system.h: \
  .scm-links gw-report-system-spec.scm ${top_builddir}/config.status
	FLAVOR=gnome $(GUILE) -c \
	 "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	  (set! %load-path (cons \"${PWD}\" %load-path)) \
	  (set! %load-path (cons \"${top_builddir}/src/engine\" %load-path)) \
	  (set! %load-path (cons \"${top_builddir}/src/app-utils\" %load-path)) \
	  (set! %load-path (cons \"${top_builddir}/src/gnome-utils\" %load-path)) \
	  (primitive-load \"./gw-report-system-spec.scm\") \
	  (gw:generate-wrapset \"gw-report-system\")"

BUILT_SOURCES = gw-report-system.scm gw-report-system.c gw-report-system.h
CLEANFILES = gnucash g-wrapped report .scm-links
DISTCLEANFILES = ${SCM_FILE_LINKS} gw-report-system.html
