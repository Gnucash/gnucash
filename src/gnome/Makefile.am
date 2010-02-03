SUBDIRS = glade schemas ui

lib_LTLIBRARIES = libgnc-gnome.la

libgnc_gnome_la_LIBADD = \
  ${top_builddir}/src/register/ledger-core/libgncmod-ledger-core.la \
  ${top_builddir}/src/register/register-gnome/libgncmod-register-gnome.la \
  ${top_builddir}/src/register/register-core/libgncmod-register-core.la \
  ${top_builddir}/src/report/report-gnome/libgncmod-report-gnome.la \
  ${top_builddir}/src/report/report-system/libgncmod-report-system.la \
  ${top_builddir}/src/gnome-search/libgncmod-gnome-search.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/backend/xml/libgnc-backend-xml-utils.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/src/html/libgncmod-html.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/core-utils/libgnc-core-utils.la \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${GLADE_LIBS} \
  ${GUILE_LIBS} \
  ${GNOME_LIBS} \
  ${GLIB_LIBS} \
  ${QOF_LIBS}

libgnc_gnome_la_SOURCES = \
  swig-gnome.c \
  dialog-chart-export.c  \
  dialog-commodities.c \
  dialog-fincalc.c \
  dialog-find-transactions.c \
  dialog-new-user.c \
  dialog-price-editor.c \
  dialog-price-edit-db.c \
  dialog-print-check.c \
  dialog-progress.c \
  dialog-sx-editor.c \
  dialog-sx-from-trans.c \
  dialog-sx-since-last-run.c \
  dialog-tax-info.c \
  dialog-userpass.c \
  druid-acct-period.c \
  druid-hierarchy.c \
  druid-merge.c \
  druid-loan.c \
  druid-stock-split.c \
  gnc-plugin-account-tree.c \
  gnc-plugin-basic-commands.c \
  gnc-plugin-budget.c \
  gnc-plugin-register.c \
  gnc-plugin-page-account-tree.c \
  gnc-plugin-page-budget.c \
  gnc-plugin-page-sx-list.c \
  gnc-plugin-page-register.c \
  gnc-split-reg.c \
  lot-viewer.c \
  reconcile-list.c \
  top-level.c \
  window-reconcile.c \
  window-autoclear.c

gnomeappdir = ${datadir}/applications

gnomeapp_in_files = gnucash.desktop.in
gnomeapp_DATA = $(gnomeapp_in_files:.desktop.in=.desktop)
@INTLTOOL_DESKTOP_RULE@

appicondir = $(datadir)/pixmaps

noinst_HEADERS = \
  dialog-chart-export.h \
  dialog-fincalc.h \
  dialog-find-transactions.h \
  dialog-new-user.h \
  dialog-print-check.h \
  dialog-progress.h \
  dialog-sx-editor.h \
  dialog-sx-from-trans.h \
  dialog-sx-since-last-run.h \
  druid-acct-period.h \
  druid-hierarchy.h \
  druid-merge.h \
  druid-loan.h \
  druid-stock-split.h \
  gnc-plugin-account-tree.h \
  gnc-plugin-basic-commands.h \
  gnc-plugin-budget.h \
  gnc-plugin-register.h \
  gnc-plugin-page-account-tree.h \
  gnc-plugin-page-budget.h \
  gnc-plugin-page-sx-list.h \
  gnc-plugin-page-register.h \
  gnc-split-reg.h \
  lot-viewer.h \
  reconcile-list.h \
  top-level.h \
  window-reconcile.h \
  window-autoclear.h

if BUILDING_FROM_SVN
swig-gnome.c: gnome.i dialog-progress.h ${top_srcdir}/src/base-typemaps.i
	$(SWIG) -guile $(SWIG_ARGS) -Linkage module \
	-I${top_srcdir}/src -o $@ $<
endif

EXTRA_DIST = \
  gnucash.desktop.in \
  gnome.i

AM_CPPFLAGS = \
  -I${top_srcdir}/src \
  -I${top_builddir}/src \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/calculation \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/backend/xml \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/gnome-search \
  -I${top_srcdir}/src/html \
  -I${top_srcdir}/src/register/ledger-core \
  -I${top_srcdir}/src/register/register-core \
  -I${top_srcdir}/src/register/register-gnome \
  -I${top_srcdir}/src/report/report-system \
  -I${top_srcdir}/src/report/report-gnome \
  -I${top_srcdir}/lib/libc \
  -I${top_srcdir}/src/libqof/qof \
  ${GUILE_INCS} \
  ${GNOME_CFLAGS} \
  ${GNOME_PRINT_CFLAGS} \
  ${GLADE_CFLAGS} \
  $(GLIB_CFLAGS)

# We build this in an earlier directory.

# The line below breaks automake 1.5.  
##OMIT_DEPENDENCIES = gnc-dir.h

CLEANFILES = gnucash.desktop
MAINTAINERCLEANFILES = swig-gnome.c

INCLUDES = -DG_LOG_DOMAIN=\"gnc.gui\"
