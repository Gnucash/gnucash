
noinst_LIBRARIES = libgncgnome.a

libgncgnome_a_SOURCES = \
  account-tree.c \
  cursors.c \
  dialog-account-picker.c \
  dialog-account.c \
  dialog-budget.c \
  dialog-commodity.c \
  dialog-dup-trans.c \
  dialog-filebox.c \
  dialog-fincalc.c \
  dialog-find-transactions.c \
  dialog-options.c \
  dialog-print-check.c \
  dialog-progress.c \
  dialog-style-sheet.c \
  dialog-tax-info.c \
  dialog-totd.c \
  dialog-transfer.c \
  dialog-utils.c \
  druid-commodity.c \
  druid-qif-import.c \
  druid-stock-split.c \
  extensions.c \
  file-history.c \
  glade-gnc-dialogs.c \
  glade-support.c \
  gnc-amount-edit.c \
  gnc-commodity-edit.c \
  gnc-currency-edit.c \
  gnc-datedelta.c \
  gnc-dateedit.c \
  gnc-gpg.c \
  gnc-html-embedded.c \
  gnc-html-history.c \
  gnc-html.c \
  gtkselect.c \
  mainwindow-account-tree.c \
  new-user-callbacks.c \
  new-user-funs.c \
  new-user-interface.c \
  print-session.c \
  query-user.c \
  reconcile-list.c \
  top-level.c \
  window-help.c \
  window-main.c \
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
  dialog-budget.h \
  dialog-commodity.h \
  dialog-fincalc.h \
  dialog-find-transactions.h \
  dialog-options.h \
  dialog-print-check.h \
  dialog-progress.h \
  dialog-style-sheet.h \
  dialog-totd.h \
  dialog-transfer.h \
  dialog-utils.h \
  druid-commodity.h \
  druid-qif-import.h \
  extensions.h \
  glade-cb-gnc-dialogs.h \
  glade-gnc-dialogs.h \
  glade-support.h \
  gnc-amount-edit.h \
  gnc-commodity-edit.h \
  gnc-currency-edit.h \
  gnc-datedelta.h \
  gnc-dateedit.h \
  gnc-gpg.h \
  gnc-html-embedded.h \
  gnc-html-history.h \
  gnc-html.h \
  gtkselect.h \
  mainwindow-account-tree.h \
  new-user-callbacks.h \
  new-user-funs.h \
  new-user-interface.h \
  print-session.h \
  query-user.h \
  reconcile-list.h \
  window-help.h \
  window-main.h \
  window-reconcile.h \
  window-register.h \
  window-report.h

GLADE_FILES = \
  glade-gnc-dialogs.c \
  glade-cb-gnc-dialogs.h \
  glade-gnc-dialogs.h \
  glade-support.c \
  glade-support.h

#${GLADE_FILES}: gnc-dialogs.glade
#	${GLADE} --write-source ${srcdir}/gnc-dialogs.glade

EXTRA_DIST = \
  .cvsignore \
  gnc-dialogs.glade \
  gnucash.desktop \
  gnucash.keys.in \
  gnucash.mime \
  new-user.glade

CFLAGS = @CFLAGS@ \
         ${G_WRAP_COMPILE_ARGS} \
         ${GNOME_PRINT_CFLAGS} \
         ${GTKHTML_CFLAGS} \
	 ${GHTTP_CFLAGS} \
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
