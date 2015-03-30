SUBDIRS = gtkbuilder gschemas ui

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
  ${LIBXML2_LIBS} \
  ${GUILE_LIBS} \
  ${GTK_LIBS} \
  ${GLIB_LIBS} \
  ${QOF_LIBS}

libgnc_gnome_la_SOURCES = \
  swig-gnome.c \
  assistant-acct-period.c \
  assistant-hierarchy.c \
  assistant-loan.c \
  assistant-stock-split.c \
  dialog-commodities.c \
  dialog-fincalc.c \
  dialog-find-transactions.c \
  dialog-find-transactions2.c \
  dialog-lot-viewer.c \
  dialog-new-user.c \
  dialog-price-editor.c \
  dialog-price-edit-db.c \
  dialog-print-check.c \
  dialog-print-check2.c \
  dialog-progress.c \
  dialog-sx-editor.c \
  dialog-sx-editor2.c \
  dialog-sx-from-trans.c \
  dialog-sx-since-last-run.c \
  dialog-tax-info.c \
  gnc-budget-view.c \
  gnc-plugin-account-tree.c \
  gnc-plugin-basic-commands.c \
  gnc-plugin-budget.c \
  gnc-plugin-register.c \
  gnc-plugin-register2.c \
  gnc-plugin-page-account-tree.c \
  gnc-plugin-page-budget.c \
  gnc-plugin-page-sx-list.c \
  gnc-plugin-page-register.c \
  gnc-plugin-page-register2.c \
  gnc-split-reg.c \
  gnc-split-reg2.c \
  reconcile-view.c \
  top-level.c \
  window-reconcile.c \
  window-reconcile2.c \
  window-autoclear.c

gnomeappdir = ${datadir}/applications

gnomeapp_in_files = gnucash.desktop.in
gnomeapp_DATA = $(gnomeapp_in_files:.desktop.in=.desktop)
@INTLTOOL_DESKTOP_RULE@

appicondir = $(datadir)/pixmaps

appdatadir = $(datadir)/appdata
appdata_in_files = gnucash.appdata.xml.in
appdata_DATA = $(appdata_in_files:.xml.in=.xml)
@INTLTOOL_XML_RULE@

noinst_HEADERS = \
  assistant-acct-period.h \
  assistant-hierarchy.h \
  assistant-loan.h \
  assistant-stock-split.h \
  dialog-fincalc.h \
  dialog-find-transactions.h \
  dialog-find-transactions2.h \
  dialog-lot-viewer.h \
  dialog-new-user.h \
  dialog-print-check.h \
  dialog-print-check2.h \
  dialog-progress.h \
  dialog-sx-editor.h \
  dialog-sx-editor2.h \
  dialog-sx-from-trans.h \
  dialog-sx-since-last-run.h \
  gnc-budget-view.h \
  gnc-plugin-account-tree.h \
  gnc-plugin-basic-commands.h \
  gnc-plugin-budget.h \
  gnc-plugin-register.h \
  gnc-plugin-register2.h \
  gnc-plugin-page-account-tree.h \
  gnc-plugin-page-budget.h \
  gnc-plugin-page-sx-list.h \
  gnc-plugin-page-register.h \
  gnc-plugin-page-register2.h \
  gnc-split-reg.h \
  gnc-split-reg2.h \
  reconcile-view.h \
  top-level.h \
  window-reconcile.h \
  window-reconcile2.h \
  window-autoclear.h

if BUILDING_FROM_VCS
swig-gnome.c: gnome.i dialog-progress.h ${top_srcdir}/src/base-typemaps.i
	$(SWIG) -guile $(SWIG_ARGS) -Linkage module \
	-I${top_srcdir}/src -o $@ $<
if ! OS_WIN32
if ! SWIG_DIST_FAIL
	if ! `grep "define scm_from_utf8_string" $@ > /dev/null 2>&1`; then \
	  patch $@ $(top_srcdir)/src/swig-utf8.patch; \
	fi
endif
endif
endif

EXTRA_DIST = \
  gnome.i \
  $(gnomeapp_in_files) \
  $(appdata_in_files)

AM_CPPFLAGS = \
  -I${top_srcdir}/src \
  -I${top_builddir}/src \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/app-utils/calculation \
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
  ${LIBXML2_CFLAGS} \
  ${GUILE_CFLAGS} \
  ${GTK_CFLAGS} \
  $(GLIB_CFLAGS) \
  ${GTK_MAC_CFLAGS}

# We build this in an earlier directory.

# The line below breaks automake 1.5.  
##OMIT_DEPENDENCIES = gnc-dir.h

CLEANFILES = gnucash.desktop gnucash.appdata.xml
MAINTAINERCLEANFILES = swig-gnome.c

AM_CPPFLAGS += -DG_LOG_DOMAIN=\"gnc.gui\"
