SUBDIRS = .  binary-import qif-import ${OFX_DIR} ${HBCI_DIR} test log-replay

pkglib_LTLIBRARIES=libgncmod-generic-import.la

libgncmod_generic_import_la_SOURCES = \
	import-account-matcher.c \
	import-commodity-matcher.c \
	import-backend.c \
	import-match-picker.c \
	import-utilities.c \
	import-settings.c \
	import-match-map.c \
	import-main-matcher.c \
	gncmod-generic-import.c

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  import-match-map.h

noinst_HEADERS = \
  import-account-matcher.h \
  import-backend.h \
  import-commodity-matcher.h \
  import-main-matcher.h \
  import-match-map.h \
  import-match-picker.h \
  import-settings.h \
  import-utilities.h 

libgncmod_generic_import_la_LDFLAGS = -module

libgncmod_generic_import_la_LIBADD = \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/app-file/libgncmod-app-file.la \
  ${top_builddir}/src/gnc-module/libgncmodule.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/register/ledger-core/libgncmod-ledger-core.la \
  ${GLADE_LIBS} \
  ${GLIB_LIBS}

gncscmdir = ${GNC_SCM_INSTALL_DIR}/generic-import

gncscm_DATA = \
	generic-import.scm

gladedir = ${GNC_GLADE_DIR}
glade_DATA = generic-import.glade

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/app-file \
  -I${top_srcdir}/src/gnome \
  -I${top_srcdir}/src/gnome-utils \
  ${G_WRAP_COMPILE_ARGS} \
  ${GNOME_INCLUDEDIR} \
  ${GTKHTML_CFLAGS} \
  ${GDK_PIXBUF_CFLAGS} \
  ${GLADE_CFLAGS} \
  ${GUILE_INCS} \
  ${GLIB_CFLAGS} 

EXTRA_DIST = \
  $(glade_DATA) \
  ${gncscm_DATA} \
  .cvsignore \
  generic-import-design.txt

#noinst_DATA = .scm-links

#.scm-links:
#	rm -f g-wrapped gnucash import-export qif-import
#	ln -sf . qif-import # to fix (load "qif-import/foo.scm")
#	ln -sf . import-export
#	ln -sf . gnucash
#	ln -sf . g-wrapped
#	touch .scm-links
DISTCLEANFILES = gnucash g-wrapped .scm-links import-export

# FIXME remove this when qif-io-core is finished
DIST_SUBDIRS = binary-import qif-import qif-io-core ofx test hbci log-replay

