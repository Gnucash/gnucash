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
	$(RM) -rf gnucash g-wrapped
	mkdir -p  gnucash g-wrapped
	mkdir -p  gnucash/report
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	( cd gnucash/report; for A in $(gncscmmod_DATA) ; do $(LN_S) -f ../../$$A . ; done )
	touch .scm-links

clean-local:
	$(RM) -rf gnucash g-wrapped

.INTERMEDIATE: gwrap-files

nodist_libgw_report_system_la_SOURCES = gw-report-system.c
libgw_report_system_la_LIBADD = \
  libgncmod-report-system.la \
  ${G_WRAP_LINK_ARGS}

gwmoddir = ${GNC_GWRAP_LIBDIR}
gwmod_DATA = gw-report-system-spec.scm
nodist_gwmod_DATA = gw-report-system.scm

noinst_DATA = .scm-links gw-report-system.h

EXTRA_DIST = ${gncscmmod_DATA} ${gncscm_DATA} ${gwmod_DATA}

gw-report-system.scm gw-report-system.c gw-report-system.h: \
  gwrap-files

gwrap-files: \
  .scm-links gw-report-system-spec.scm ${top_builddir}/config.status
	FLAVOR=gnome $(GUILE) -c \
	 "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	  (set! %load-path (cons \"${PWD}\" %load-path)) \
	  (set! %load-path (cons \"${top_builddir}/src/engine\" %load-path)) \
	  (set! %load-path (cons \"${top_builddir}/src/app-utils\" %load-path)) \
	  (set! %load-path (cons \"${top_builddir}/src/gnome-utils\" %load-path)) \
	  (primitive-load \"./gw-report-system-spec.scm\") \
	  (gw:generate-wrapset \"gw-report-system\")"
	touch $@
	( cd g-wrapped; $(LN_S) -f ../gw-*.scm . )

BUILT_SOURCES = gw-report-system.scm gw-report-system.c gw-report-system.h
CLEANFILES = $(BUILT_SOURCES) .scm-links \
	${SCM_FILE_LINKS} gw-report-system.html
