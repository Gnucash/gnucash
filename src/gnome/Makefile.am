
noinst_LIBRARIES = libgncgnome.a

libgncgnome_a_SOURCES = \
  top-level.c \
  window-main.c \
  window-register.c \
  window-adjust.c \
  window-help.c \
  cursors.c \
  account-tree.c \
  dialog-budget.c \
  window-reconcile.c \
  window-html.c \
  dialog-options.c \
  dialog-filebox.c \
  dialog-transfer.c \
  dialog-add.c \
  dialog-edit.c \
  dialog-utils.c \
  extensions.c \
  query-user.c \
  reconcile-list.c \
  window-report.c \
  gnc-dateedit.c gnc-currency-edit.c \
  dialog-qif-import.c \
  glade-gnc-dialogs.c \
  gnc-datedelta.c \
  dialog-account-picker.c \
  print-session.c \
  file-history.c \
  dialog-print-check.c \
  dialog-find-transactions.c

gnomeappdir = ${datadir}/gnome/apps/Applications

gnomeapp_DATA = gnucash.desktop

noinst_HEADERS = \
  account-tree.h \
  account-treeP.h \
  cursors.h \
  dialog-account-picker.h \
  dialog-budget.h \
  dialog-edit.h \
  dialog-find-transactions.h \
  dialog-options.h \
  dialog-print-check.h \
  dialog-qif-import.h \
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
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/g-wrap \
  -I${top_srcdir}/src/guile \
  -I${top_srcdir}/src/register \
  -I${top_srcdir}/src/register/gnome
