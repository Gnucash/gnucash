SUBDIRS = gtkbuilder gschemas ui

lib_LTLIBRARIES = libgnc-gnome.la

libgnc_gnome_la_LIBADD = \
  ${top_builddir}/gnucash/register/ledger-core/libgncmod-ledger-core.la \
  ${top_builddir}/gnucash/register/register-gnome/libgncmod-register-gnome.la \
  ${top_builddir}/gnucash/register/register-core/libgncmod-register-core.la \
  ${top_builddir}/gnucash/report/report-gnome/libgncmod-report-gnome.la \
  ${top_builddir}/gnucash/report/report-system/libgncmod-report-system.la \
  ${top_builddir}/gnucash/gnome-search/libgncmod-gnome-search.la \
  ${top_builddir}/gnucash/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/libgnucash/backend/xml/libgnc-backend-xml-utils.la \
  ${top_builddir}/libgnucash/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/gnucash/html/libgncmod-html.la \
  ${top_builddir}/libgnucash/engine/libgncmod-engine.la \
  ${top_builddir}/libgnucash/core-utils/libgnc-core-utils.la \
  ${LIBXML2_LIBS} \
  ${GUILE_LIBS} \
  ${GTK_LIBS} \
  ${GLIB_LIBS}

libgnc_gnome_la_SOURCES = \
  swig-gnome.c \
  assistant-acct-period.c \
  assistant-hierarchy.c \
  assistant-loan.c \
  assistant-stock-split.c \
  business-options-gnome.c \
  business-urls.c \
  business-gnome-utils.c \
  dialog-billterms.c \
  dialog-choose-owner.c \
  dialog-commodities.c \
  dialog-customer.c \
  dialog-date-close.c \
  dialog-employee.c \
  dialog-fincalc.c \
  dialog-find-account.c \
  dialog-find-transactions.c \
  dialog-find-transactions2.c \
  dialog-imap-editor.c \
  dialog-invoice.c \
  dialog-job.c \
  dialog-lot-viewer.c \
  dialog-new-user.c \
  dialog-order.c \
  dialog-payment.c \
  dialog-price-editor.c \
  dialog-price-edit-db.c \
  dialog-print-check.c \
  dialog-progress.c \
  dialog-sx-editor.c \
  dialog-sx-editor2.c \
  dialog-sx-from-trans.c \
  dialog-sx-since-last-run.c \
  dialog-tax-info.c \
  dialog-trans-assoc.c \
  dialog-vendor.c \
  gnc-budget-view.c \
  gnc-plugin-account-tree.c \
  gnc-plugin-basic-commands.c \
  gnc-plugin-budget.c \
  gnc-plugin-business.c \
  gnc-plugin-page-account-tree.c \
  gnc-plugin-page-budget.c \
  gnc-plugin-page-invoice.c \
  gnc-plugin-page-owner-tree.c \
  gnc-plugin-page-sx-list.c \
  gnc-plugin-page-register.c \
  gnc-plugin-page-register2.c \
  gnc-plugin-register.c \
  gnc-plugin-register2.c \
  gnc-split-reg.c \
  gnc-split-reg2.c \
  reconcile-view.c \
  search-owner.c \
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
  business-options-gnome.h \
  business-urls.h \
  business-gnome-utils.h \
  dialog-billterms.h \
  dialog-choose-owner.h \
  dialog-customer.h \
  dialog-date-close.h \
  dialog-employee.h \
  dialog-fincalc.h \
  dialog-find-account.h \
  dialog-find-transactions.h \
  dialog-find-transactions2.h \
  dialog-imap-editor.h \
  dialog-invoice.h \
  dialog-job.h \
  dialog-lot-viewer.h \
  dialog-new-user.h \
  dialog-order.h \
  dialog-payment.h \
  dialog-print-check.h \
  dialog-progress.h \
  dialog-sx-editor.h \
  dialog-sx-editor2.h \
  dialog-sx-from-trans.h \
  dialog-sx-since-last-run.h \
  dialog-trans-assoc.h \
  dialog-vendor.h \
  gnc-budget-view.h \
  gnc-plugin-account-tree.h \
  gnc-plugin-basic-commands.h \
  gnc-plugin-budget.h \
  gnc-plugin-business.h \
  gnc-plugin-page-account-tree.h \
  gnc-plugin-page-budget.h \
  gnc-plugin-page-invoice.h \
  gnc-plugin-page-owner-tree.h \
  gnc-plugin-page-sx-list.h \
  gnc-plugin-page-register.h \
  gnc-plugin-page-register2.h \
  gnc-plugin-register.h \
  gnc-plugin-register2.h \
  gnc-split-reg.h \
  gnc-split-reg2.h \
  reconcile-view.h \
  search-owner.h \
  top-level.h \
  window-reconcile.h \
  window-reconcile2.h \
  window-autoclear.h

if BUILDING_FROM_VCS
swig-gnome.c: gnome.i dialog-progress.h ${top_srcdir}/common/base-typemaps.i
	$(SWIG) -guile -Linkage module \
	-I${top_srcdir}/common -o $@ $<
endif

EXTRA_DIST = \
  gnome.i \
  $(gnomeapp_in_files) \
  $(appdata_in_files) \
  CMakeLists.txt

AM_CPPFLAGS = \
  -I${top_srcdir}/common \
  -I${top_builddir}/common \
  -I${top_srcdir}/libgnucash/core-utils \
  -I${top_srcdir}/libgnucash/engine \
  -I${top_srcdir}/libgnucash/app-utils \
  -I${top_srcdir}/libgnucash/app-utils/calculation \
  -I${top_srcdir}/libgnucash/backend/xml \
  -I${top_srcdir}/gnucash/gnome-utils \
  -I${top_builddir}/gnucash/gnome-utils \
  -I${top_srcdir}/gnucash/gnome-search \
  -I${top_srcdir}/gnucash/html \
  -I${top_srcdir}/gnucash/register/ledger-core \
  -I${top_srcdir}/gnucash/register/register-core \
  -I${top_srcdir}/gnucash/register/register-gnome \
  -I${top_srcdir}/gnucash/report/report-system \
  -I${top_srcdir}/gnucash/report/report-gnome \
  -I${top_srcdir}/borrowed/libc \
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
