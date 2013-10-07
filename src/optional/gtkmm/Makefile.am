SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-gtkmm.la

libgncmod_gtkmm_la_SOURCES = \
  gncmm/Account.cpp \
  gncmm/Book.cpp \
  gncmm/Commodity.cpp \
  gncmm/GncInstance.cpp \
  gncmm/Numeric.cpp \
  gncmm/Split.cpp \
  gncmm/Transaction.cpp \
  gncmm/wrap_init.cpp \
  gnc-plugin-gtkmm.cpp \
  gncmod-gtkmm.cpp

noinst_HEADERS = \
  gncmm/Account.hpp \
  gncmm/Book.hpp \
  gncmm/Commodity.hpp \
  gncmm/GncInstance.hpp \
  gncmm/Numeric.hpp \
  gncmm/Split.hpp \
  gncmm/Transaction.hpp \
  gncmm/private/Account_p.hpp \
  gncmm/private/Book_p.hpp \
  gncmm/private/Commodity_p.hpp \
  gncmm/private/GncInstance_p.hpp \
  gncmm/private/Split_p.hpp \
  gncmm/private/Transaction_p.hpp \
  gncmm/wrap_init.hpp \
  gnc-plugin-gtkmm.hpp

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
  ${GLIB_LIBS}

AM_CPPFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/libqof/qof \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/engine \
  ${GTKMM_CFLAGS} \
  ${GLIB_CFLAGS}

uidir = $(GNC_UI_DIR)
ui_DATA = gnc-plugin-gtkmm-ui.xml

gtkbuilderdir = $(GNC_GTKBUILDER_DIR)
gtkbuilder_DATA =

EXTRA_DIST = ${ui_DATA} ${gtkbuilder_DATA}

AM_CPPFLAGS += -DG_LOG_DOMAIN=\"gnc.gui.gtkmm\"
