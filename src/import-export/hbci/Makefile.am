SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-hbci.la

libgncmod_hbci_la_SOURCES = \
  gncmod-hbci.c 

libgncmod_hbci_la_LDFLAGS = -module
libgncmod_hbci_la_LIBADD = \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/app-file/libgncmod-app-file.la \
  ${top_builddir}/src/gnc-module/libgncmodule.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${GUILE_LIBS} \
  ${GLIB_LIBS}

AM_CFLAGS = -I${top_srcdir}/src/gnc-module \
  ${GLIB_CFLAGS} \
  ${GUILE_INCS}


.scm-links:
	rm -f gnucash import-export
	ln -sf . gnucash
	ln -sf . import-export 
	touch .scm-links

noinst_DATA = .scm-links

gncscmmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash/import-export
gncscmmod_DATA = hbci.scm

gncscmdir = ${GNC_SHAREDIR}/scm
gncscm_DATA = 

EXTRA_DIST = \
  ${gncscmmod_DATA} \
  ${gncscm_DATA}

CLEANFILES = ${SCM_FILE_LINKS} gnucash import-export .scm-links
