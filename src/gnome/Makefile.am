
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

CFLAGS += ${GNOME_CFLAGS} ${G_WRAP_COMPILE_ARGS}

INCLUDES = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/g-wrap \
  -I${top_srcdir}/src/guile \
  -I${top_srcdir}/src/register \
  -I${top_srcdir}/src/register/gnome
