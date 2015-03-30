SUBDIRS = . gtkbuilder gschemas ui

pkglib_LTLIBRARIES = libgncmod-business-gnome.la

AM_CPPFLAGS = \
  -I${top_srcdir}/src \
  -I${top_builddir}/src \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/gnome \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/gnome-search \
  -I${top_srcdir}/src/html \
  -I${top_srcdir}/src/report/report-gnome \
  -I${top_srcdir}/src/business/business-ledger \
  -I${top_srcdir}/src/register/register-core \
  -I${top_srcdir}/src/register/register-gnome \
  -I${top_srcdir}/src/register/ledger-core \
  -I${top_srcdir}/src/libqof/qof \
  ${GTK_CFLAGS} \
  ${GLIB_CFLAGS} \
  ${GUILE_CFLAGS}

libgncmod_business_gnome_la_SOURCES = \
  business-options-gnome.c \
  business-urls.c \
  business-gnome-utils.c \
  dialog-billterms.c \
  dialog-choose-owner.c \
  dialog-customer.c \
  dialog-date-close.c \
  dialog-employee.c \
  dialog-invoice.c \
  dialog-job.c \
  dialog-order.c \
  dialog-payment.c \
  dialog-vendor.c \
  gnc-plugin-business.c \
  gnc-plugin-page-invoice.c \
  gnc-plugin-page-owner-tree.c \
  gncmod-business-gnome.c \
  search-owner.c

noinst_HEADERS = \
  business-options-gnome.h \
  business-urls.h \
  business-gnome-utils.h \
  dialog-billterms.h \
  dialog-choose-owner.h \
  dialog-customer.h \
  dialog-date-close.h \
  dialog-employee.h \
  dialog-invoice.h \
  dialog-job.h \
  dialog-order.h \
  dialog-payment.h \
  dialog-vendor.h \
  gnc-plugin-business.h \
  gnc-plugin-page-invoice.h \
  gnc-plugin-page-owner-tree.h \
  search-owner.h

libgncmod_business_gnome_la_LDFLAGS = -avoid-version

libgncmod_business_gnome_la_LIBADD = \
  ${top_builddir}/src/gnome/libgnc-gnome.la \
  ${top_builddir}/src/business/business-ledger/libgnc-business-ledger.la \
  ${top_builddir}/src/register/register-core/libgncmod-register-core.la \
  ${top_builddir}/src/register/register-gnome/libgncmod-register-gnome.la \
  ${top_builddir}/src/report/report-gnome/libgncmod-report-gnome.la \
  ${top_builddir}/src/gnome-search/libgncmod-gnome-search.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/src/html/libgncmod-html.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/core-utils/libgnc-core-utils.la \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${GTK_LIBS} \
  ${GUILE_LIBS} \
  ${GLIB_LIBS} \
  ${EFENCE_LIBS}

gncscmmoddir = ${GNC_SCM_INSTALL_DIR}/gnucash
gncscmmod_DATA = business-gnome.scm

EXTRA_DIST = \
  ${gncscmmod_DATA}

if GNUCASH_SEPARATE_BUILDDIR
#Only needed when srcdir and builddir are different
#for running
SCM_FILE_LINKS = ${gncscmmod_DATA}
endif

.scm-links: 
	$(RM) -rf gnucash
	mkdir -p  gnucash
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	( cd gnucash; for A in $(gncscmmod_DATA) ; do $(LN_S) -f ../$$A . ; done )
if ! OS_WIN32
# Windows knows no "ln -s" but uses "cp": must copy every time (see bug #566567).
	touch .scm-links
endif

if GNC_HAVE_GUILE_2
GUILE_COMPILE_ENV = \
  --gnc-module-dir ${top_builddir}/src/app-utils \
  --gnc-module-dir ${top_builddir}/src/engine \
  --gnc-module-dir ${top_builddir}/src/html \
  --gnc-module-dir ${top_builddir}/src/gnome-utils \
  --gnc-module-dir ${top_builddir}/src/report/report-system \
  --gnc-module-dir ${top_builddir}/src/report/standard-reports \
  --guile-load-dir ${top_builddir}/src/app-utils \
  --guile-load-dir ${top_builddir}/src/core-utils \
  --guile-load-dir ${top_builddir}/src/engine \
  --guile-load-dir ${top_builddir}/src/gnc-module \
  --guile-load-dir ${top_builddir}/src/gnome-utils \
  --guile-load-dir ${top_builddir}/src/report/business-reports \
  --guile-load-dir ${top_builddir}/src/report/report-system \
  --guile-load-dir ${top_builddir}/src/report/standard-reports \
  --guile-load-dir ${top_builddir}/src/scm \
  --library-dir    ${top_builddir}/src/libqof/qof \
  --library-dir    ${top_builddir}/src/engine \
  --library-dir    ${top_builddir}/src/app-utils \
  --library-dir    ${top_builddir}/src/core-utils \
  --library-dir    ${top_builddir}/src/gnc-module \
  --library-dir    ${top_builddir}/src/gnome-utils \
  --library-dir    ${top_builddir}/src/backend/xml \
  --library-dir    ${top_builddir}/src/backend/sql \
  --library-dir    ${top_builddir}/src/html \
  --library-dir    ${top_builddir}/src/report/report-system


%.go : %.scm .scm-links $(pkglib_LTLIBRARIES)
	GNC_UNINSTALLED=yes \
	GNC_BUILDDIR=${top_builddir} \
	$(shell ${abs_top_srcdir}/src/gnc-test-env.pl --noexports ${GUILE_COMPILE_ENV}) \
	$(GUILD) compile -o $@ $<

gncscmmodcachedir = ${pkglibdir}/scm/ccache/@GUILE_EFFECTIVE_VERSION@/gnucash
gncscmmodcache_DATA = $(gncscmmod_DATA:.scm=.go)
endif

noinst_DATA = .scm-links
clean-local:
	rm -rf gnucash

CLEANFILES = \
	$(BUILT_SOURCES) \
	.scm-links \
	${SCM_FILE_LINKS} \
	${gncscmmodcache_DATA}

AM_CPPFLAGS += -DG_LOG_DOMAIN=\"gnc.business.gnome\"
