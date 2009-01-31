SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-locale-reports-us.la

libgncmod_locale_reports_us_la_SOURCES = \
  gncmod-locale-reports-us.c 

libgncmod_locale_reports_us_la_LDFLAGS = -avoid-version

libgncmod_locale_reports_us_la_LIBADD = \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${GUILE_LIBS} \
  ${GLIB_LIBS} 


AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnc-module \
  ${GUILE_INCS} ${GLIB_CFLAGS}

## This is unused and therefore no longer installed and/or loaded
gncscmmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash/report/locale-specific
gncscmmod_DATA = us.scm de_DE.scm

gncscmothermoddir = ${GNC_SHAREDIR}/guile-modules/gnucash/report/
gncscmothermod_DATA = taxtxf.scm taxtxf-de_DE.scm

if GNUCASH_SEPARATE_BUILDDIR
#For executing test cases
SCM_FILE_LINKS = \
  ${gncscmmod_DATA} \
  ${gncscmothermod_DATA}
endif

.scm-links:
	$(RM) -rf gnucash
	mkdir -p  gnucash
	mkdir -p  gnucash/report
	mkdir -p  gnucash/report/locale-specific
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	( cd gnucash/report/locale-specific; for A in $(gncscmmod_DATA) ; do $(LN_S) -f ../../../$$A . ; done )
	( cd gnucash/report; for A in $(gncscmothermod_DATA) ; do $(LN_S) -f ../../$$A . ; done )
if ! OS_WIN32
# Windows knows no "ln -s" but uses "cp": must copy every time (see bug #566567).
	touch .scm-links
endif

clean-local:
	$(RM) -rf gnucash

noinst_DATA = .scm-links

EXTRA_DIST = \
  ${gncscmothermod_DATA} \
  ${gncscmmod_DATA} 

CLEANFILES = .scm-links
DISTCLEANFILES = ${SCM_FILE_LINKS}

INCLUDES = -DG_LOG_DOMAIN=\"gnc.report.locale.us\"
