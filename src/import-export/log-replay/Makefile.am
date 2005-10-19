SUBDIRS = .

pkglib_LTLIBRARIES=libgncmod-log-replay.la

libgncmod_log_replay_la_SOURCES = \
  gnc-log-replay.c \
  gncmod-log-replay.c

noinst_HEADERS = \
  gnc-log-replay.h

libgncmod_log_replay_la_LDFLAGS = -module

libgncmod_log_replay_la_LIBADD = \
  ${top_builddir}/src/gnc-module/libgncmodule.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/import-export/libgncmod-generic-import.la \
  ${GLIB_LIBS}

gncscmdir = ${GNC_SCM_INSTALL_DIR}/log-replay

gncscm_DATA = \
	log-replay.scm

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/app-file \
  -I${top_srcdir}/src/gnome \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/import-export \
  ${GNOME_INCLUDEDIR} \
  ${GTKHTML_CFLAGS} \
  ${GLADE_CFLAGS} \
  ${GUILE_INCS} \
  ${GLIB_CFLAGS}

EXTRA_DIST = \
  .cvsignore \
  ${gncscm_DATA}

CLEANFILES = g-wrapped .scm-links
