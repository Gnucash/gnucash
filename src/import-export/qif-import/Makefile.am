
pkglib_LTLIBRARIES=libgncmod-qif-import.la

libgncmod_qif_import_la_SOURCES = \
  dialog-account-picker.c \
  druid-qif-import.c \
  gncmod-qif-import.c

noinst_HEADERS = \
  dialog-account-picker.h \
  druid-qif-import.h

libgncmod_qif_import_la_LDFLAGS = \
  -module \
  ${GNOME_LIBS} \
  ${GLADE_LIBS} \
  ${GLIB_LIBS} \
  ${GUILE_LIBS} 

gncscmmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash/import-export
gncscmmod_DATA = qif-import.scm 

gncscmdir = ${GNC_SCM_INSTALL_DIR}/qif-import

gncscm_DATA = \
  qif-dialog-utils.scm \
  qif-file.scm \
  qif-guess-map.scm \
  qif-merge-groups.scm \
  qif-objects.scm \
  qif-parse.scm \
  qif-to-gnc.scm \
  qif-utils.scm \
  simple-obj.scm

gladedir = ${GNC_GLADE_DIR}
glade_DATA = qif.glade

STRING_FILES = qif_strings.c 

CFLAGS = @CFLAGS@ ${G_WRAP_COMPILE_ARGS}

INCLUDES = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/guile \
  -I${top_srcdir}/src/gnome \
  ${GLIB_CFLAGS} \
  ${GNOME_INCLUDEDIR} \
  ${GUILE_INCS}

EXTRA_DIST = \
  .cvsignore \
  ${gncscm_DATA} \
  ${glade_DATA} \
  ${STRING_FILES} \
  file-format.txt

guile-strings.c: ${gncscm_DATA}
	rm -f guile-strings.c
	guile -s ../../scm/xgettext.scm ${gncscm_DATA}
CLEANFILES += guile-strings.c

all-local: guile-strings.c
