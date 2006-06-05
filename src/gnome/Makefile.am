if !HAVE_GLIB26
  GLIB26_LIB = ${top_builddir}/lib/glib26/libgncglib.la
  GLIB26_INC = -I${top_srcdir}/lib/glib26
endif

SUBDIRS = glade schemas ui

lib_LTLIBRARIES = libgncgnome.la libgw-gnc.la 

gwmoddir = ${GNC_GWRAP_LIBDIR}
gwmod_DATA = gw-gnc-spec.scm gw-gnc.scm
#nodist_gwmod_DATA = gw-gnc.scm

libgncgnome_la_LIBADD = \
  ${top_builddir}/src/register/ledger-core/libgncmod-ledger-core.la \
  ${top_builddir}/src/register/register-gnome/libgncmod-register-gnome.la \
  ${top_builddir}/src/register/register-core/libgncmod-register-core.la \
  ${top_builddir}/src/report/report-gnome/libgncmod-report-gnome.la \
  ${top_builddir}/src/report/report-system/libgncmod-report-system.la \
  ${top_builddir}/src/gnome-search/libgncmod-gnome-search.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/backend/file/libgnc-backend-file-utils.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/src/engine/libgw-engine.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/calculation/libgncmod-calculation.la \
  ${top_builddir}/src/core-utils/libcore-utils.la \
  ${GLIB26_LIBS} \
  ${GLADE_LIBS} \
  ${G_WRAP_LINK_ARGS} \
  ${GUILE_LIBS} ${GNOME_LIBS} ${GLIB_LIBS} ${QOF_LIBS}

libgw_gnc_la_SOURCES = gw-gnc.c

libgw_gnc_la_LDFLAGS =  \
  ${G_WRAP_LINK_ARGS} ${GLIB_LIBS}
libgw_gnc_la_LIBADD = libgncgnome.la

libgncgnome_la_SOURCES = \
  dialog-chart-export.c  \
  dialog-commodities.c \
  dialog-fincalc.c \
  dialog-find-transactions.c \
  dialog-new-user.c \
  dialog-price-editor.c \
  dialog-price-edit-db.c \
  dialog-print-check.c \
  dialog-progress.c \
  dialog-sx-from-trans.c \
  dialog-sxsincelast.c \
  dialog-tax-info.c \
  dialog-userpass.c \
  dialog-scheduledxaction.c \
  druid-acct-period.c \
  druid-hierarchy.c \
  druid-merge.c \
  druid-loan.c \
  druid-stock-split.c \
  gnc-plugin-account-tree.c \
  gnc-plugin-basic-commands.c \
  gnc-plugin-budget.c \
  gnc-plugin-register.c \
  gnc-plugin-page-account-tree.c \
  gnc-plugin-page-budget.c \
  gnc-plugin-page-register.c \
  gnc-split-reg.c \
  lot-viewer.c \
  reconcile-list.c \
  top-level.c \
  window-reconcile.c

gnomeappdir = ${datadir}/applications

gnomeapp_in_files = gnucash.desktop.in
gnomeapp_DATA = $(gnomeapp_in_files:.desktop.in=.desktop)
@INTLTOOL_DESKTOP_RULE@

appicondir = $(datadir)/pixmaps

noinst_HEADERS = \
  dialog-chart-export.h \
  dialog-fincalc.h \
  dialog-find-transactions.h \
  dialog-new-user.h \
  dialog-print-check.h \
  dialog-progress.h \
  dialog-sx-from-trans.h \
  dialog-sxsincelast.h \
  dialog-scheduledxaction.h \
  druid-acct-period.h \
  druid-hierarchy.h \
  druid-merge.h \
  druid-loan.h \
  druid-stock-split.h \
  gnc-plugin-account-tree.h \
  gnc-plugin-basic-commands.h \
  gnc-plugin-budget.h \
  gnc-plugin-register.h \
  gnc-plugin-page-account-tree.h \
  gnc-plugin-page-budget.h \
  gnc-plugin-page-register.h \
  gnc-split-reg.h \
  gw-gnc.h \
  lot-viewer.h \
  reconcile-list.h \
  top-level.h \
  window-reconcile.h \
  gw-gnc.h

EXTRA_DIST = \
  gnucash.desktop.in \
  ${gwmod_DATA}


AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/calculation \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/network-utils \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/backend/file \
  -I${top_srcdir}/src/g-wrap \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/gnome-search \
  -I${top_srcdir}/src/register/ledger-core \
  -I${top_srcdir}/src/register/register-core \
  -I${top_srcdir}/src/register/register-gnome \
  -I${top_srcdir}/src/report/report-system \
  -I${top_srcdir}/src/report/report-gnome \
  ${GLIB26_INC} \
  -I${top_srcdir}/lib/libc \
  ${GUILE_INCS} \
  ${G_WRAP_COMPILE_ARGS} \
  ${GNOME_CFLAGS} \
  ${GNOME_PRINT_CFLAGS} \
  ${GDK_PIXBUF_CFLAGS} \
  ${GTKHTML_CFLAGS} \
  ${GLADE_CFLAGS} \
  ${QOF_CFLAGS}

# We build this in an earlier directory.

# The line below breaks automake 1.5.  
##OMIT_DEPENDENCIES = gnc-dir.h

noinst_DATA = .scm-links

if GNUCASH_SEPARATE_BUILDDIR
#For compiling
SCM_FILE_LINKS = gw-gnc-spec.scm
endif

.scm-links: 
	rm -f gnucash g-wrapped
	$(LN_S) -f . gnucash 
	$(LN_S) -f . g-wrapped 
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	touch .scm-links

gw-gnc.scm gw-gnc.h gw-gnc.c gw-gnc.html: \
  gw-gnc-spec.scm .scm-links ${top_builddir}/config.status Makefile
	FLAVOR=gnome $(GUILE) -c \
          "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
           (set! %load-path (cons \"../engine\" %load-path)) \
           (set! %load-path (cons \"../app-utils\" %load-path)) \
           (set! %load-path (cons \"../gnome-utils\" %load-path)) \
           (set! %load-path (cons \"../report/report-gnome\" %load-path)) \
           (primitive-load \"./gw-gnc-spec.scm\") \
           (gw:generate-wrapset \"gw-gnc\")"
BUILT_SOURCES = gw-gnc.scm gw-gnc.h gw-gnc.c
CLEANFILES = gnucash.desktop gnucash g-wrapped .scm-links
DISTCLEANFILES = ${SCM_FILE_LINKS} gw-gnc.html
