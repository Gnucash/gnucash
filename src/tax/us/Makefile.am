SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-tax-us.la

libgncmod_tax_us_la_SOURCES = \
  gncmod-tax-us.c 

libgncmod_tax_us_la_LDFLAGS = -module

INCLUDES = -I${top_srcdir}/src/gnc-module ${GLIB_CFLAGS}

.scm-links:
	rm -f gnucash tax
	ln -sf . gnucash
	ln -sf . tax 
	touch .scm-links

noinst_DATA = .scm-links

gncscmmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash/tax
gncscmmod_DATA = us.scm

gncscmdir = ${GNC_SHAREDIR}/scm
gncscm_DATA = txf.scm txf-help.scm 

EXTRA_DIST = \
  ${gncscmmod_DATA} \
  ${gncscm_DATA}

CLEANFILES = gnucash tax .scm-links
