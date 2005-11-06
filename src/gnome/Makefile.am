SUBDIRS = glade schemas ui

lib_LTLIBRARIES = libgncgnome.la libgw-gnc.la 

gwmoddir = ${GNC_GWRAP_LIBDIR}
gwmod_DATA = gw-gnc-spec.scm gw-gnc.scm
#nodist_gwmod_DATA = gw-gnc.scm

libgncgnome_la_LIBADD = \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/report/report-system/libgncmod-report-system.la \
  ${top_builddir}/src/report/report-gnome/libgncmod-report-gnome.la \
  ${top_builddir}/src/register/ledger-core/libgncmod-ledger-core.la \
  ${top_builddir}/src/gnome-search/libgncmod-gnome-search.la \
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
  dialog-totd.c \
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
  window-main-summarybar.c \
  window-reconcile.c \
  ${top_srcdir}/src/backend/file/sixtp-dom-parsers.c \
  ${top_srcdir}/src/backend/file/sixtp-dom-generators.c \
  ${top_srcdir}/src/backend/file/sixtp-utils.c \
  ${top_srcdir}/src/backend/file/sixtp.c \
  ${top_srcdir}/src/backend/file/sixtp-stack.c \
  ${top_srcdir}/src/backend/file/sixtp-to-dom-parser.c \
  ${top_srcdir}/src/backend/file/io-example-account.c \
  ${top_srcdir}/src/backend/file/io-gncxml-gen.c \
  ${top_srcdir}/src/backend/file/io-gncxml-v2.c \
  ${top_srcdir}/src/backend/file/io-utils.c \
  ${top_srcdir}/src/backend/file/gnc-account-xml-v2.c \
  ${top_srcdir}/src/backend/file/gnc-budget-xml-v2.c \
  ${top_srcdir}/src/backend/file/gnc-lot-xml-v2.c \
  ${top_srcdir}/src/backend/file/gnc-recurrence-xml-v2.c \
  ${top_srcdir}/src/backend/file/gnc-schedxaction-xml-v2.c \
  ${top_srcdir}/src/backend/file/gnc-freqspec-xml-v2.c \
  ${top_srcdir}/src/backend/file/gnc-transaction-xml-v2.c \
  ${top_srcdir}/src/backend/file/gnc-commodity-xml-v2.c \
  ${top_srcdir}/src/backend/file/gnc-book-xml-v2.c \
  ${top_srcdir}/src/backend/file/gnc-pricedb-xml-v2.c


gnomeappdir = ${datadir}/gnome/apps/Applications

gnomeapp_in_files = gnucash.desktop.in
gnomeapp_DATA = $(gnomeapp_in_files:.desktop.in=.desktop)
@INTLTOOL_DESKTOP_RULE@

appicondir = $(datadir)/pixmaps

mimedir = $(datadir)/mime-info
mime_DATA = gnucash.keys gnucash.mime

noinst_HEADERS = \
  dialog-chart-export.h \
  dialog-fincalc.h \
  dialog-find-transactions.h \
  dialog-new-user.h \
  dialog-print-check.h \
  dialog-progress.h \
  dialog-sx-from-trans.h \
  dialog-sxsincelast.h \
  dialog-totd.h \
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
  window-main-summarybar.h \
  window-reconcile.h \
  gw-gnc.h

EXTRA_DIST = \
  gnucash.desktop.in \
  gnucash.keys.in \
  gnucash.mime \
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
  ${GUILE_INCS} \
  ${G_WRAP_COMPILE_ARGS} \
  ${GNOME_CFLAGS} \
  ${GNOME_PRINT_CFLAGS} \
  ${GDK_PIXBUF_CFLAGS} \
  ${GTKHTML_CFLAGS} \
  ${GLADE_CFLAGS} \
  ${QOF_CFLAGS} \
  ${GUPPI_CFLAGS} 

# We build this in an earlier directory.

# The line below breaks automake 1.5.  
##OMIT_DEPENDENCIES = gnc-dir.h

## We borrow guile's convention and use @-...-@ as the substitution
## brackets here, instead of the usual @...@.  This prevents autoconf
## from substituting the values directly into the left-hand sides of
## the sed substitutions.
gnucash.keys: gnucash.keys.in ${top_builddir}/config.status
	rm -f $@.tmp
	sed < $< > $@.tmp \
            -e 's:@-icondir-@:${appicondir}:g'
	chmod +x $@.tmp
	mv $@.tmp $@

noinst_DATA = .scm-links

if GNUCASH_SEPARATE_BUILDDIR
#For compiling
SCM_FILE_LINKS = gw-gnc-spec.scm
endif

.scm-links: 
	rm -f gnucash g-wrapped
	ln -sf . gnucash 
	ln -sf . g-wrapped 
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  ln -sf ${srcdir}/$$X . ; \
	done
endif
	touch .scm-links

gw-gnc.scm gw-gnc.h gw-gnc.c gw-gnc.html: \
  gw-gnc-spec.scm .scm-links ${top_builddir}/config.status
	FLAVOR=gnome $(GUILE) -c \
          "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
           (set! %load-path (cons \"../engine\" %load-path)) \
           (set! %load-path (cons \"../app-utils\" %load-path)) \
           (set! %load-path (cons \"../gnome-utils\" %load-path)) \
           (set! %load-path (cons \"../report/report-gnome\" %load-path)) \
           (primitive-load \"./gw-gnc-spec.scm\") \
           (gw:generate-wrapset \"gw-gnc\")"
BUILT_SOURCES = gw-gnc.scm gw-gnc.h gw-gnc.c
CLEANFILES = gnucash.keys gnucash.desktop
DISTCLEANFILES = gnucash g-wrapped .scm-links ${SCM_FILE_LINKS} \
                 gw-gnc.html
