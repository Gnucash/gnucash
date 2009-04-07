SUBDIRS = . test schemas

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

libgncmod_qif_import_la_LDFLAGS = -avoid-version

libgncmod_qif_import_la_LIBADD = \
  ${top_builddir}/src/import-export/libgncmod-generic-import.la \
  ${top_builddir}/src/gnome/libgnc-gnome.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/src/core-utils/libgnc-core-utils.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${GUILE_LIBS} \
  ${GNOME_LIBS} \
  ${GLADE_LIBS} \
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

AM_CPPFLAGS = \
  -I${top_srcdir}/src \
  -I${top_builddir}/src \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/gnome \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/import-export \
  -I${top_srcdir}/src/libqof/qof \
  ${GUILE_INCS} \
  ${GLIB_CFLAGS} \
  ${GLADE_CFLAGS} \
  ${GNOME_CFLAGS} \
  ${GDK_PIXBUF_CFLAGS} \
  ${GTKHTML_CFLAGS}

uidir = $(GNC_UI_DIR)
ui_DATA = \
	gnc-plugin-qif-import-ui.xml

EXTRA_DIST = \
  ${gncscm_DATA} \
  ${glade_DATA} \
  file-format.txt \
  $(ui_DATA)

CLEANFILES =

if GNUCASH_SEPARATE_BUILDDIR
SCM_FILE_LINKS = \
  ${gncscm_DATA}
endif

noinst_DATA = .scm-links

.scm-links: 
	$(RM) -rf gnucash
	mkdir -p  gnucash qif-import
	mkdir -p  gnucash/import-export
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	( cd gnucash/import-export; for A in $(gncscmmod_DATA) ; do $(LN_S) -f ../../$$A . ; done )
	( cd qif-import; for A in $(gncscm_DATA) ; do $(LN_S) -f ../$$A . ; done )
if ! OS_WIN32
# Windows knows no "ln -s" but uses "cp": must copy every time (see bug #566567).
	touch .scm-links
endif

clean-local:
	$(RM) -rf gnucash qif-import

CLEANFILES += .scm-links
DISTCLEANFILES = ${SCM_FILE_LINKS}

INCLUDES = -DG_LOG_DOMAIN=\"gnc.import.qif.import\"
