SUBDIRS = glade

noinst_LIBRARIES = libgncgnome.a

libgncgnome_a_SOURCES = \
  argv-list-converters.c \
  dialog-account.c \
  dialog-column-view.c \
  dialog-commodities.c \
  dialog-dup-trans.c \
  dialog-filebox.c \
  dialog-fincalc.c \
  dialog-find-transactions.c \
  dialog-new-user.c \
  dialog-options.c \
  dialog-price-editor.c \
  dialog-print-check.c \
  dialog-progress.c \
  dialog-style-sheet.c \
  dialog-sx-from-trans.c \
  dialog-tax-info.c \
  dialog-totd.c \
  dialog-transfer.c \
  dialog-userpass.c \
  dialog-scheduledxaction.c \
  druid-commodity.c \
  druid-hierarchy.c \
  druid-stock-split.c \
  file-history.c \
  file-utils.c \
  gnc-network.c \
  gnc-splash.c \
  mainwindow-account-tree.c \
  reconcile-list.c \
  top-level.c \
  window-acct-tree.c \
  window-help.c \
  window-main.c \
  window-main-summarybar.c \
  window-reconcile.c \
  window-register.c \
  window-report.c \
  dialog-nextrun.c \
  dialog-sxsincelast.c

gnomeappdir = ${datadir}/gnome/apps/Applications

gnomeapp_DATA = gnucash.desktop

appicondir = $(datadir)/pixmaps

mimedir = $(datadir)/mime-info
mime_DATA = gnucash.keys gnucash.mime

noinst_HEADERS = \
  argv-list-converters.h \
  dialog-account.h \
  dialog-column-view.h \
  dialog-fincalc.h \
  dialog-find-transactions.h \
  dialog-new-user.h \
  dialog-options.h \
  dialog-print-check.h \
  dialog-progress.h \
  dialog-style-sheet.h \
  dialog-sx-from-trans.h \
  dialog-totd.h \
  dialog-transfer.h \
  dialog-scheduledxaction.h \
  druid-commodity.h \
  druid-hierarchy.h \
  file-utils.h \
  gnc-network.h \
  gnc-splash.h \
  mainwindow-account-tree.h \
  reconcile-list.h \
  top-level.h \
  window-acct-tree.h \
  window-help.h \
  window-main.h \
  window-main-summarybar.h \
  window-reconcile.h \
  window-register.h \
  window-report.h \
  dialog-nextrun.h \
  dialog-sxsincelast.h

EXTRA_DIST = \
  .cvsignore \
  gnucash.desktop \
  gnucash.keys.in \
  gnucash.mime

CFLAGS = @CFLAGS@ \
         ${G_WRAP_COMPILE_ARGS} \
         ${GNOME_PRINT_CFLAGS} \
         ${GTKHTML_CFLAGS} \
	 ${GHTTP_CFLAGS} \
         ${GLADE_CFLAGS} \
	 ${GUPPI_CFLAGS} 

INCLUDES = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/calculation \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/network-utils \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/backend/file \
  -I${top_srcdir}/src/g-wrap \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/guile \
  -I${top_srcdir}/src/register/ledger-core \
  -I${top_srcdir}/src/register/register-core \
  -I${top_srcdir}/src/register/register-gnome \
  -I${top_srcdir}/src/report/report-system \
  -I${top_srcdir}/src/import-export/qif-import \
  ${GNOME_INCLUDEDIR} \
  ${GUILE_INCS}


## We borrow guile's convention and use @-...-@ as the substitution
## brackets here, instead of the usual @...@.  This prevents autoconf
## from substituting the values directly into the left-hand sides of
## the sed substitutions.
gnucash.keys: gnucash.keys.in
	rm -f $@.tmp
	sed < $@.in > $@.tmp \
            -e 's:@-icondir-@:${appicondir}:g'
	chmod +x $@.tmp
	mv $@.tmp $@
