SUBDIRS = .  binary-import qif-import ${OFX_DIR} test

pkglib_LTLIBRARIES=libgncmod-generic-import.la

libgncmod_generic_import_la_SOURCES = \
  gnc-generic-import.c

noinst_HEADERS = \
  gnc-generic-import.h

libgncmod_generic_import_la_LDFLAGS = -module

libgncmod_generic_import_la_LIBADD = \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/app-file/libgncmod-app-file.la \
  ${top_builddir}/src/gnc-module/libgncmodule.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/dialog-account/libdialog-account.la \
  ${top_builddir}/src/register/ledger-core/libgncmod-ledger-core.la \
  ${GLADE_LIBS} \
  ${GLIB_LIBS}

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
  -I${top_srcdir}/src/dialog-account \
  ${G_WRAP_COMPILE_ARGS} \
  ${GLIB_CFLAGS} \
  ${GLADE_CFLAGS} \
  ${GDK_PIXBUF_CFLAGS} \
  ${GTKHTML_CFLAGS}

EXTRA_DIST = \
  .cvsignore

CLEANFILES =

#noinst_DATA = .scm-links

#.scm-links:
#	rm -f g-wrapped gnucash import-export qif-import
#	ln -sf . qif-import # to fix (load "qif-import/foo.scm")
#	ln -sf . import-export
#	ln -sf . gnucash
#	ln -sf . g-wrapped
#	touch .scm-links
CLEANFILES += generic-import import-export gnucash g-wrapped .scm-links

# FIXME remove this when qif-io-core is finished
DIST_SUBDIRS = binary-import qif-import qif-io-core ofx
