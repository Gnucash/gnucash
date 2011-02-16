SUBDIRS =

pkglib_LTLIBRARIES = libgncmod-gtkmm.la

libgncmod_gtkmm_la_SOURCES = \
  gncmod-gtkmm.cpp

noinst_HEADERS =

libgncmod_gtkmm_la_LDFLAGS = -avoid-version

libgncmod_gtkmm_la_LIBADD = \
  ${top_builddir}/src/gnome/libgnc-gnome.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/core-utils/libgnc-core-utils.la \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${GTKMM_LIBS} \
  ${GNOME_LIBS} \
  ${GLADE_LIBS} \
  ${GLIB_LIBS}

AM_CPPFLAGS = \
  -I${top_srcdir}/src \
  ${GTKMM_CFLAGS} \
  ${GNOME_CFLAGS} \
  ${GLADE_CFLAGS} \
  ${GLIB_CFLAGS}

uidir = $(GNC_UI_DIR)
ui_DATA =

gladedir = $(GNC_GLADE_DIR)
glade_DATA =

EXTRA_DIST = ${ui_DATA} ${glade_DATA}

INCLUDES = -DG_LOG_DOMAIN=\"gnc.gtkmm\"
