SUBDIRS = glade

lib_LTLIBRARIES = libgncgnome.la libgw-gnc.la

gwmoddir = ${GNC_GWRAP_LIBDIR}
gwmod_DATA = gw-gnc-spec.scm gw-gnc.scm

libgw_gnc_la_SOURCES = gw-gnc.c
libgw_gnc_la_LIBADD = ${G_WRAP_LINK_ARGS} ${GLIB_LIBS}
libgw_gnc_la_LDFLAGS = -module

libgncgnome_la_LDFLAGS = -module
libgncgnome_la_LIBADD = ../guile/libgncguile.la ${GNOMEUI_LIBS} ${GLIB_LIBS} ${GUILE_LIBS}

libgncgnome_la_SOURCES = \
  argv-list-converters.c \
  dialog-account.c \
  dialog-column-view.c \
  dialog-commodities.c \
  dialog-dup-trans.c \
  dialog-fincalc.c \
  dialog-find-transactions.c \
  dialog-new-user.c \
  dialog-price-editor.c \
  dialog-print-check.c \
  dialog-progress.c \
  dialog-style-sheet.c \
  dialog-sx-from-trans.c \
  dialog-sxsincelast.c \
  dialog-tax-info.c \
  dialog-totd.c \
  dialog-transfer.c \
  dialog-userpass.c \
  dialog-scheduledxaction.c \
  druid-hierarchy.c \
  druid-stock-split.c \
  file-utils.c \
  gnc-network.c \
  gnc-splash.c \
  mainwindow-account-tree.c \
  reconcile-list.c \
  tip-of-the-day.c \
  top-level.c \
  window-acct-tree.c \
  window-help.c \
  window-main.c \
  window-main-summarybar.c \
  window-reconcile.c \
  window-register.c \
  window-report.c

gnomeappdir = ${datadir}/gnome/apps/Applications

gnomeapp_in_files = gnucash.desktop.in
gnomeapp_DATA = $(gnomeapp_in_files:.desktop.in=.desktop)
@INTLTOOL_DESKTOP_RULE@

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
  dialog-print-check.h \
  dialog-progress.h \
  dialog-style-sheet.h \
  dialog-sx-from-trans.h \
  dialog-sxsincelast.h \
  dialog-totd.h \
  dialog-transfer.h \
  dialog-scheduledxaction.h \
  druid-hierarchy.h \
  file-utils.h \
  gnc-network.h \
  gnc-splash.h \
  mainwindow-account-tree.h \
  reconcile-list.h \
  tip-of-the-day.h \
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
  gnucash.desktop.in \
  gnucash.keys.in \
  gnucash.mime

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/calculation \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/network-utils \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/app-file \
  -I${top_srcdir}/src/app-file/gnome \
  -I${top_srcdir}/src/backend/file \
  -I${top_srcdir}/src/g-wrap \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/guile \
  -I${top_srcdir}/src/register/ledger-core \
  -I${top_srcdir}/src/register/register-core \
  -I${top_srcdir}/src/register/register-gnome \
  -I${top_srcdir}/src/report/report-system \
  -I${top_srcdir}/src/import-export/binary-import \
  -I${top_srcdir}/src/import-export/qif-import \
  ${GUILE_INCS} \
  ${G_WRAP_COMPILE_ARGS} \
  ${GNOME_INCLUDEDIR} \
  ${GNOME_PRINT_CFLAGS} \
  ${GDK_PIXBUF_CFLAGS} \
  ${GTKHTML_CFLAGS} \
  ${GHTTP_CFLAGS} \
  ${GLADE_CFLAGS} \
  ${GUPPI_CFLAGS} 

# We build this in an earlier directory.

# The line below breaks automake 1.5.  
##OMIT_DEPENDENCIES = gnc-dir.h

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

noinst_DATA = .scm-links

.scm-links: 
	rm -f gnucash g-wrapped
	ln -sf . gnucash 
	ln -sf . g-wrapped 
	touch .scm-links
CLEANFILES = .scm-links gnucash g-wrapped

gw-gnc.scm gw-gnc.h gw-gnc.c gw-gnc.html: gw-gnc-spec.scm .scm-links
	FLAVOR=gnome guile -c \
          "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
           (set! %load-path (cons \"../engine\" %load-path)) \
           (primitive-load \"./gw-gnc-spec.scm\") \
           (gw:generate-module \"gw-gnc\")"
BUILT_SOURCES = gw-gnc.scm gw-gnc.h gw-gnc.c
CLEANFILES += gw-gnc.scm gw-gnc.h gw-gnc.c 
