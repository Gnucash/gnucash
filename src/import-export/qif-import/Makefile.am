SUBDIRS = . test

pkglib_LTLIBRARIES=libgncmod-qif-import.la

libgncmod_qif_import_la_SOURCES = \
  dialog-account-picker.c \
  druid-qif-import.c \
  gnc-plugin-qif-import.c \
  gnc-druid-test.c \
  gncmod-qif-import.c

noinst_HEADERS = \
  dialog-account-picker.h \
  druid-qif-import.h \
  gnc-druid-test.h \
  gnc-plugin-qif-import.h

libgncmod_qif_import_la_LIBADD = \
  ${top_builddir}/src/import-export/libgncmod-generic-import.la \
  ${top_builddir}/src/core-utils/libcore-utils.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/gnc-module/libgncmodule.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${GUILE_LIBS} \
  ${GNOME_LIBS} \
  ${GLADE_LIBS} \
  ${QOF_LIBS} \
  ${GLIB_LIBS}


gncscmmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash/import-export
gncscmmod_DATA = qif-import.scm 

gncscmdir = ${GNC_SCM_INSTALL_DIR}/qif-import

gncscm_DATA = \
  qif-dialog-utils.scm \
  qif-file.scm \
  qif-guess-map.scm \
  qif-import.scm \
  qif-merge-groups.scm \
  qif-objects.scm \
  qif-parse.scm \
  qif-to-gnc.scm \
  qif-utils.scm

gladedir = ${GNC_GLADE_DIR}
glade_DATA = qif.glade

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/gnome \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/import-export \
  ${G_WRAP_COMPILE_ARGS} \
  ${GUILE_INCS} \
  ${GLIB_CFLAGS} \
  ${GLADE_CFLAGS} \
  ${GNOME_PRINT_CFLAGS} \
  ${GNOME_CFLAGS} \
  ${GDK_PIXBUF_CFLAGS} \
  ${QOF_CFLAGS} \
  ${GTKHTML_CFLAGS}

uidir = $(GNC_UI_DIR)
ui_DATA = \
	gnc-plugin-qif-import-ui.xml

EXTRA_DIST = \
  .cvsignore \
  ${gncscm_DATA} \
  ${glade_DATA} \
  file-format.txt \
  $(ui_DATA)

CLEANFILES =

noinst_DATA = .scm-links

.scm-links: 
	rm -f g-wrapped gnucash import-export qif-import
	ln -sf . qif-import # to fix (load "qif-import/foo.scm")
	ln -sf . import-export
	ln -sf . gnucash 
	ln -sf . g-wrapped 
	touch .scm-links
CLEANFILES += qif-import import-export gnucash g-wrapped .scm-links
