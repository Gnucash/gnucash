
noinst_LIBRARIES = libgncgnome.a

libgncgnome_a_SOURCES = \
  account-tree.c \
  cursors.c \
  dialog-account.c \
  dialog-account-picker.c \
  dialog-budget.c \
  dialog-commodity.c \
  dialog-filebox.c \
  dialog-fincalc.c \
  dialog-find-transactions.c \
  dialog-options.c \
  dialog-print-check.c \
  dialog-progress.c \
  dialog-totd.c \
  dialog-transfer.c \
  dialog-utils.c \
  druid-commodity.c \
  druid-qif-import.c \
  extensions.c \
  file-history.c \
  glade-gnc-dialogs.c \
  gnc-amount-edit.c \
  gnc-commodity-edit.c \
  gnc-currency-edit.c \
  gnc-datedelta.c \
  gnc-dateedit.c \
  gtkselect.c \
  mainwindow-account-tree.c \
  print-session.c \
  query-user.c \
  reconcile-list.c \
  top-level.c \
  window-main.c \
  window-reconcile.c \
  window-register.c \
  gnc-html.c \
  gnc-html-history.c \
  gnc-html-embedded.c \
  window-help.c \
  window-report.c 

gnomeappdir = ${datadir}/gnome/apps/Applications

gnomeapp_DATA = gnucash.desktop

appicondir = $(datadir)/pixmaps

mimedir = $(datadir)/mime-info
mime_DATA = gnucash.keys gnucash.mime

noinst_HEADERS = \
  account-tree.h \
  cursors.h \
  dialog-account.h \
  dialog-account-picker.h \
  dialog-budget.h \
  dialog-commodity.h \
  dialog-fincalc.h \
  dialog-find-transactions.h \
  dialog-options.h \
  dialog-print-check.h \
  dialog-progress.h \
  dialog-totd.h \
  dialog-transfer.h \
  dialog-utils.h \
  druid-commodity.h \
  druid-qif-import.h \
  extensions.h \
  glade-cb-gnc-dialogs.h \
  glade-gnc-dialogs.h \
  glade-support-gnc-dialogs.h \
  gnc-amount-edit.h \
  gnc-commodity-edit.h \
  gnc-currency-edit.h \
  gnc-datedelta.h \
  gnc-dateedit.h \
  gtkselect.h \
  print-session.h \
  query-user.h \
  reconcile-list.h \
  window-main.h \
  window-reconcile.h \
  window-register.h \
  gnc-html.h \
  gnc-html-history.h \
  gnc-html-embedded.h \
  window-help.h \
  window-report.h


GLADE_FILES = \
  glade-gnc-dialogs.c \
  glade-cb-gnc-dialogs.h \
  glade-gnc-dialogs.h \
  glade-support-gnc-dialogs.h

#${GLADE_FILES}: gnc-dialogs.glade
#	${GLADE} --write-source ${srcdir}/gnc-dialogs.glade

EXTRA_DIST = \
  .cvsignore \
  gnc-dialogs.glade \
  gnucash.desktop \
  gnucash.keys.in \
  gnucash.mime

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
