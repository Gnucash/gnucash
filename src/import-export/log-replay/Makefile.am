SUBDIRS = .

pkglib_LTLIBRARIES=libgncmod-log-replay.la

libgncmod_log_replay_la_SOURCES = \
  gnc-log-replay.c \
  gnc-plugin-log-replay.c \
  gncmod-log-replay.c

noinst_HEADERS = \
  gnc-log-replay.h \
  gnc-plugin-log-replay.h

libgncmod_log_replay_la_LDFLAGS = -module

libgncmod_log_replay_la_LIBADD = \
  ${top_builddir}/src/gnc-module/libgncmodule.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/import-export/libgncmod-generic-import.la \
  ${top_builddir}/lib/egg/libegg.la \
  ${GLIB_LIBS}

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/app-file \
  -I${top_srcdir}/src/gnome \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/import-export \
  -I${top_srcdir}/lib/egg \
  ${GNOME_CFLAGS} \
  ${GTKHTML_CFLAGS} \
  ${GLADE_CFLAGS} \
  ${GUILE_INCS} \
  ${GLIB_CFLAGS}

uidir = $(GNC_UI_DIR)
ui_DATA = \
	gnc-plugin-log-replay-ui.xml

EXTRA_DIST = \
  .cvsignore

CLEANFILES = g-wrapped
