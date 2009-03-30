SUBDIRS = .

pkglib_LTLIBRARIES=libgncmod-log-replay.la

libgncmod_log_replay_la_SOURCES = \
  gnc-log-replay.c \
  gnc-plugin-log-replay.c \
  gncmod-log-replay.c

noinst_HEADERS = \
  gnc-log-replay.h \
  gnc-plugin-log-replay.h

libgncmod_log_replay_la_LDFLAGS = -avoid-version

libgncmod_log_replay_la_LIBADD = \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/core-utils/libgnc-core-utils.la \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${GTK_LIBS} \
  ${GLIB_LIBS} ${QOF_LIBS}

AM_CPPFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/gnome \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/import-export \
  ${QOF_CFLAGS} \
  ${GNOME_CFLAGS} \
  ${GLADE_CFLAGS} \
  ${GUILE_INCS} \
  ${GLIB_CFLAGS}

uidir = $(GNC_UI_DIR)
dist_ui_DATA = \
	gnc-plugin-log-replay-ui.xml

INCLUDES = -DG_LOG_DOMAIN=\"gnc.import.log-replay\"
