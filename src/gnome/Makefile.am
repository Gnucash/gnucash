SUBDIRS = glade

noinst_LIBRARIES = libgncgnome.a

libgncgnome_a_SOURCES = \
  account-tree.c \
  cursors.c \
  dialog-account-picker.c \
  dialog-account.c \
  dialog-column-view.c \
  dialog-commodities.c \
  dialog-commodity.c \
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
  dialog-tax-info.c \
  dialog-totd.c \
  dialog-transfer.c \
  dialog-userpass.c \
  dialog-utils.c \
  druid-commodity.c \
  druid-hierarchy.c \
  druid-qif-import.c \
  druid-stock-split.c \
  druid-utils.c \
  extensions.c \
  file-history.c \
  gnc-amount-edit.c \
  gnc-commodity-edit.c \
  gnc-currency-edit.c \
  gnc-datedelta.c \
  gnc-dateedit.c \
  gnc-gpg.c \
  gnc-html-history.c \
  gnc-html-guppi.c \
  gnc-html.c \
  gnc-http.c \
  gnc-network.c \
  gnc-splash.c \
  gtkselect.c \
  mainwindow-account-tree.c \
  print-session.c \
  query-user.c \
  reconcile-list.c \
  top-level.c \
  window-acct-tree.c \
  window-help.c \
  window-main.c \
  window-main-summarybar.c \
  window-reconcile.c \
  window-register.c \
  window-report.c

gnomeappdir = ${datadir}/gnome/apps/Applications

gnomeapp_DATA = gnucash.desktop

appicondir = $(datadir)/pixmaps

mimedir = $(datadir)/mime-info
mime_DATA = gnucash.keys gnucash.mime

noinst_HEADERS = \
  account-tree.h \
  dialog-account-picker.h \
  dialog-account.h \
  dialog-column-view.h \
  dialog-commodity.h \
  dialog-fincalc.h \
  dialog-find-transactions.h \
  dialog-new-user.h \
  dialog-options.h \
  dialog-print-check.h \
  dialog-progress.h \
  dialog-style-sheet.h \
  dialog-totd.h \
  dialog-transfer.h \
  dialog-utils.h \
  druid-commodity.h \
  druid-hierarchy.h \
  druid-qif-import.h \
  druid-utils.h \
  extensions.h \
  gnc-amount-edit.h \
  gnc-commodity-edit.h \
  gnc-currency-edit.h \
  gnc-datedelta.h \
  gnc-dateedit.h \
  gnc-dir.h \
  gnc-gpg.h \
  gnc-html-history.h \
  gnc-html-guppi.h \
  gnc-html.h \
  gnc-http.h \
  gnc-network.h \
  gnc-splash.h \
  gtkselect.h \
  mainwindow-account-tree.h \
  print-session.h \
  query-user.h \
  reconcile-list.h \
  top-level.h \
  window-acct-tree.h \
  window-help.h \
  window-main.h \
  window-main-summarybar.h \
  window-reconcile.h \
  window-register.h \
  window-report.h

EXTRA_DIST = \
  .cvsignore \
  gnc-dir.h.in \
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
  -I${top_srcdir}/src/g-wrap \
  -I${top_srcdir}/src/guile \
  -I${top_srcdir}/src/register \
  -I${top_srcdir}/src/register/gnome \
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

## We borrow guile's convention and use @-...-@ as the substitution
## brackets here, instead of the usual @...@.  This prevents autoconf
## from substituting the values directly into the left-hand sides of
## the sed substitutions.
gnc-dir.h: gnc-dir.h.in
	rm -f $@.tmp
	sed < $@.in > $@.tmp \
	    -e 's:@-GNC_ACCOUNTS_DIR-@:${GNC_ACCOUNTS_DIR}:g' \
	    -e 's:@-GNC_GLADE_DIR-@:${GNC_GLADE_DIR}:g' \
	    -e 's:@-GNC_PIXMAP_DIR-@:${GNC_PIXMAP_DIR}:g'
	mv $@.tmp $@
BUILT_SOURCES += gnc-dir.h

# We have to do this because otherwise automake insists on putting
# these files into the dist tarfile.  If there's a a better way,
# by all means, let us know...
dist-hook:
	rm ${distdir}/gnc-dir.h
