SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-locale-reports-us.la

libgncmod_locale_reports_us_la_SOURCES = \
  gncmod-locale-reports-us.c 

libgncmod_locale_reports_us_la_LDFLAGS = -module

INCLUDES = -I${top_srcdir}/src/gnc-module ${GLIB_CFLAGS}

.scm-links:
	rm -f gnucash report locale-specific us 
	ln -sf . gnucash
	ln -sf . report 
	ln -sf . locale-specific 
	ln -sf . us 
	touch .scm-links

noinst_DATA = .scm-links

gncscmmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash/report/locale-specific
gncscmmod_DATA = us.scm 

gncscmothermoddir = ${GNC_SHAREDIR}/guile-modules/gnucash/report/
gncscmothermod_DATA = taxtxf.scm 

EXTRA_DIST = \
  ${gncscmmod_DATA} \
  ${gncscmothermod_DATA}

CLEANFILES += gnucash report locale-specific us .scm-links
