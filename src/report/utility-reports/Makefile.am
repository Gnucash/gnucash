SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-utility-reports.la

libgncmod_utility_reports_la_SOURCES = \
  gncmod-utility-reports.c 
libgncmod_utility_reports_la_LIBADD = \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${GUILE_LIBS} \
  ${GLIB_LIBS}

libgncmod_utility_reports_la_LDFLAGS = -module -avoid-version

AM_CPPFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnc-module \
  ${GUILE_INCS} ${GLIB_CFLAGS}

gncscmmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash/report/
gncscmmod_DATA = \
  hello-world.scm \
  utility-reports.scm \
  view-column.scm \
  welcome-to-gnucash.scm

#  test-graphing.scm -- not intended for general public?!? Surely not translated at all.

if GNUCASH_SEPARATE_BUILDDIR
#For separate build directory
SCM_FILE_LINKS = \
  ${gncscmmod_DATA}
endif

.scm-links:
	$(RM) -rf gnucash
	mkdir -p  gnucash
	mkdir -p  gnucash/report
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	( cd gnucash/report; for A in $(gncscmmod_DATA) ; do $(LN_S) -f ../../$$A . ; done )
if ! OS_WIN32
# Windows knows no "ln -s" but uses "cp": must copy every time (see bug #566567).
	touch .scm-links
endif

clean-local:
	$(RM) -rf gnucash

noinst_DATA = .scm-links

EXTRA_DIST = ${gncscmmod_DATA}

CLEANFILES = .scm-links
DISTCLEANFILES = ${SCM_FILE_LINKS}

INCLUDES = -DG_LOG_DOMAIN=\"gnc.report.util\"
