SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-stylesheets.la

libgncmod_stylesheets_la_SOURCES = \
  gnc-plugin-stylesheets.c \
  gncmod-stylesheets.c 

noinst_HEADERS = \
  gnc-plugin-stylesheets.h

libgncmod_stylesheets_la_LDFLAGS = -module -avoid-version

libgncmod_stylesheets_la_LIBADD = \
  ${top_builddir}/src/report/report-gnome/libgncmod-report-gnome.la \
  ${top_builddir}/src/report/report-system/libgncmod-report-system.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${GUILE_LIBS} \
  ${GTK_LIBS} \
  ${GLIB_LIBS}

AM_CPPFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/gnome \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/report/report-gnome \
  -I${top_srcdir}/src/libqof/qof \
  ${GUILE_CFLAGS} \
  ${GLIB_CFLAGS} \
  ${GTK_CFLAGS}

gncscmmoddir = ${GNC_SCM_INSTALL_DIR}/gnucash/report/
gncscmmod_DATA = \
  stylesheets.scm \
  stylesheet-plain.scm \
  stylesheet-fancy.scm \
  stylesheet-footer.scm \
  stylesheet-easy.scm \
  stylesheet-head-or-tail.scm

if GNUCASH_SEPARATE_BUILDDIR
#For executing test cases
SCM_FILE_LINKS = ${gncscmmod_DATA}
endif

.scm-links:
	$(RM) -rf gnucash
	mkdir -p  gnucash
	mkdir -p  gnucash/report
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	( cd gnucash/report; for A in $(gncscmmod_DATA) ; do $(LN_S) -f ../../$$A . ; done )
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
  --guile-load-dir ${top_builddir}/src/app-utils \
  --guile-load-dir ${top_builddir}/src/core-utils \
  --guile-load-dir ${top_builddir}/src/engine \
  --guile-load-dir ${top_builddir}/src/gnc-module \
  --guile-load-dir ${top_builddir}/src/gnome-utils \
  --guile-load-dir ${top_builddir}/src/report/report-system \
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

gncscmmodcachedir = ${pkglibdir}/scm/ccache/@GUILE_EFFECTIVE_VERSION@/gnucash/report
gncscmmodcache_DATA = $(gncscmmod_DATA:.scm=.go)
endif

clean-local:
	$(RM) -rf gnucash

noinst_DATA = .scm-links

uidir = $(GNC_UI_DIR)
dist_ui_DATA = \
	gnc-plugin-stylesheets-ui.xml

EXTRA_DIST = ${gncscmmod_DATA}

CLEANFILES = .scm-links ${gncscmmodcache_DATA}
DISTCLEANFILES = ${SCM_FILE_LINKS}

AM_CPPFLAGS += -DG_LOG_DOMAIN=\"gnc.report.core\"
