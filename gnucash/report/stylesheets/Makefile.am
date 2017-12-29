SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-stylesheets.la

libgncmod_stylesheets_la_SOURCES = \
  gnc-plugin-stylesheets.c \
  gncmod-stylesheets.c 

noinst_HEADERS = \
  gnc-plugin-stylesheets.h

libgncmod_stylesheets_la_LDFLAGS = -module -avoid-version

libgncmod_stylesheets_la_LIBADD = \
  ${top_builddir}/gnucash/report/report-gnome/libgncmod-report-gnome.la \
  ${top_builddir}/gnucash/report/report-system/libgncmod-report-system.la \
  ${top_builddir}/gnucash/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/libgnucash/gnc-module/libgnc-module.la \
  ${top_builddir}/libgnucash/engine/libgncmod-engine.la \
  ${GUILE_LIBS} \
  ${GTK_LIBS} \
  ${GLIB_LIBS}

AM_CPPFLAGS = \
  -I${top_srcdir}/common \
  -I${top_srcdir}/libgnucash/gnc-module \
  -I${top_srcdir}/libgnucash/engine \
  -I${top_srcdir}/libgnucash/app-utils \
  -I${top_srcdir}/gnucash/gnome \
  -I${top_srcdir}/gnucash/gnome-utils \
  -I${top_srcdir}/gnucash/report/report-gnome \
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

GUILE_COMPILE_ENV = \
  --gnc-module-dir ${top_builddir}/libgnucash/app-utils \
  --gnc-module-dir ${top_builddir}/libgnucash/engine \
  --gnc-module-dir ${top_builddir}/gnucash/html \
  --gnc-module-dir ${top_builddir}/gnucash/gnome-utils \
  --gnc-module-dir ${top_builddir}/gnucash/report/report-system \
  --gnc-module-dir ${top_builddir}/gnucash/report/stylesheets \
  --guile-load-dir ${top_builddir}/libgnucash/app-utils \
  --guile-load-dir ${top_builddir}/libgnucash/core-utils \
  --guile-load-dir ${top_builddir}/libgnucash/engine \
  --guile-load-dir ${top_builddir}/libgnucash/gnc-module \
  --guile-load-dir ${top_builddir}/gnucash/gnome-utils \
  --guile-load-dir ${top_builddir}/gnucash/report/report-system \
  --guile-load-dir ${top_builddir}/gnucash/report/stylesheets \
  --guile-load-dir ${top_builddir}/libgnucash/scm \
  --library-dir    ${top_builddir}/libgnucash/engine \
  --library-dir    ${top_builddir}/libgnucash/app-utils \
  --library-dir    ${top_builddir}/libgnucash/core-utils \
  --library-dir    ${top_builddir}/libgnucash/gnc-module \
  --library-dir    ${top_builddir}/gnucash/gnome-utils \
  --library-dir    ${top_builddir}/libgnucash/backend/xml \
  --library-dir    ${top_builddir}/libgnucash/backend/sql \
  --library-dir    ${top_builddir}/gnucash/html \
  --library-dir    ${top_builddir}/gnucash/report/report-system \
  --library-dir    ${top_builddir}/gnucash/report/stylesheets

%.go : %.scm .scm-links $(pkglib_LTLIBRARIES)
	GNC_UNINSTALLED=yes \
	GNC_BUILDDIR=${top_builddir} \
	$(shell ${abs_top_srcdir}/common/gnc-test-env.pl --noexports ${GUILE_COMPILE_ENV}) \
	$(GUILD) compile -o $@ $<

gncscmmodcachedir = ${pkglibdir}/scm/ccache/@GUILE_EFFECTIVE_VERSION@/gnucash/report
gncscmmodcache_DATA = $(gncscmmod_DATA:.scm=.go)

clean-local:
	$(RM) -rf gnucash

noinst_DATA = .scm-links

uidir = $(GNC_UI_DIR)
dist_ui_DATA = \
	gnc-plugin-stylesheets-ui.xml

EXTRA_DIST = ${gncscmmod_DATA} CMakeLists.txt

CLEANFILES = .scm-links ${gncscmmodcache_DATA}
DISTCLEANFILES = ${SCM_FILE_LINKS}

AM_CPPFLAGS += -DG_LOG_DOMAIN=\"gnc.report.core\"
