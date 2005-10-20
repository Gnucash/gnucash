SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-stylesheets.la

libgncmod_stylesheets_la_SOURCES = \
  gnc-plugin-stylesheets.c \
  gncmod-stylesheets.c 

noinst_HEADERS = \
  gnc-plugin-stylesheets.h

libgncmod_stylesheets_la_LDFLAGS=-module

libgncmod_stylesheets_la_LIBADD = \
  ${QOF_LIBS} \
  ${top_builddir}/src/report/report-system/libgncmod-report-system.la

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/gnome \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/report/report-gnome \
  ${GUILE_INCS} \
  ${GLIB_CFLAGS} \
  ${QOF_CFLAGS} \
  ${GNOME_CFLAGS}

.scm-links:
	rm -f gnucash report
	ln -sf . gnucash
	ln -sf ${srcdir} report 
	touch .scm-links

noinst_DATA = .scm-links

gncscmmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash/report/
gncscmmod_DATA = \
  stylesheets.scm \
  stylesheet-plain.scm \
  stylesheet-fancy.scm \
  stylesheet-easy.scm

uidir = $(GNC_UI_DIR)
dist_ui_DATA = \
	gnc-plugin-stylesheets-ui.xml

EXTRA_DIST = ${gncscmmod_DATA}

CLEANFILES = gnucash report .scm-links

