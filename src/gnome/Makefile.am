
noinst_LIBRARIES = libgncgnome.a

libgncgnome_a_SOURCES = \
  account-tree.c \
  cursors.c \
  dialog-account.c \
  dialog-account-picker.c \
  dialog-budget.c \
  dialog-filebox.c \
  dialog-fincalc.c \
  dialog-find-transactions.c \
  dialog-options.c \
  dialog-print-check.c \
  dialog-qif-import.c \
  dialog-totd.c \
  dialog-transfer.c \
  dialog-utils.c \
  extensions.c \
  file-history.c \
  glade-gnc-dialogs.c \
  gnc-currency-edit.c \
  gnc-datedelta.c \
  gnc-dateedit.c \
  gtkselect.c \
  print-session.c \
  query-user.c \
  reconcile-list.c \
  top-level.c \
  window-adjust.c \
  window-help.c \
  window-html.c \
  window-main.c \
  window-reconcile.c \
  window-register.c \
  window-report.c

gnomeappdir = ${datadir}/gnome/apps/Applications

gnomeapp_DATA = gnucash.desktop

noinst_HEADERS = \
  account-tree.h \
  account-treeP.h \
  cursors.h \
  dialog-account.h \
  dialog-account-picker.h \
  dialog-budget.h \
  dialog-fincalc.h \
  dialog-find-transactions.h \
  dialog-options.h \
  dialog-print-check.h \
  dialog-qif-import.h \
  dialog-totd.h \
  dialog-transfer.h \
  dialog-utils.h \
  extensions.h \
  glade-cb-gnc-dialogs.h \
  glade-gnc-dialogs.h \
  glade-support-gnc-dialogs.h \
  gnc-currency-edit.h \
  gnc-datedelta.h \
  gnc-dateedit.h \
  gnome-top-level.h \
  gtkselect.h \
  print-session.h \
  query-user.h \
  reconcile-list.h \
  reconcile-listP.h \
  window-help.h \
  window-html.h \
  window-main.h \
  window-mainP.h \
  window-reconcile.h \
  window-register.h \
  window-report.h

EXTRA_DIST = \
  .cvsignore \
  gnc-dialogs.glade \
  gnucash.desktop

CFLAGS = @CFLAGS@ ${GNOME_CFLAGS} ${G_WRAP_COMPILE_ARGS} ${GUILE_COMPILE_ARGS}

INCLUDES = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/calculation \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/g-wrap \
  -I${top_srcdir}/src/guile \
  -I${top_srcdir}/src/register \
  -I${top_srcdir}/src/register/gnome
