SUBDIRS = . test gschemas

pkglib_LTLIBRARIES=libgncmod-qif-import.la

libgncmod_qif_import_la_SOURCES = \
  dialog-account-picker.c \
  assistant-qif-import.c \
  gnc-plugin-qif-import.c \
  gncmod-qif-import.c

noinst_HEADERS = \
  dialog-account-picker.h \
  assistant-qif-import.h \
  gnc-plugin-qif-import.h

libgncmod_qif_import_la_LDFLAGS = -avoid-version

libgncmod_qif_import_la_LIBADD = \
  ${top_builddir}/gnucash/import-export/libgncmod-generic-import.la \
  ${top_builddir}/gnucash/gnome/libgnc-gnome.la \
  ${top_builddir}/gnucash/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/libgnucash/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/libgnucash/core-utils/libgnc-core-utils.la \
  ${top_builddir}/libgnucash/engine/libgncmod-engine.la \
  ${top_builddir}/libgnucash/gnc-module/libgnc-module.la \
  ${GUILE_LIBS} \
  ${GLIB_LIBS}


gncscmmoddir = ${GNC_SCM_INSTALL_DIR}/gnucash/import-export
gncscmmod_DATA = qif-import.scm 

gncscmdir = ${GNC_SCM_INSTALL_DIR}/qif-import

gncscm_DATA = \
  qif-dialog-utils.scm \
  qif-file.scm \
  qif-guess-map.scm \
  qif-import.scm \
  qif-merge-groups.scm \
  qif-objects.scm \
  qif-parse.scm \
  qif-to-gnc.scm \
  qif-utils.scm

gtkbuilderdir = ${GNC_GTKBUILDER_DIR}
gtkbuilder_DATA = \
   dialog-account-picker.glade \
   assistant-qif-import.glade

AM_CPPFLAGS = \
  -I${top_srcdir}/common \
  -I${top_builddir}/common \
  -I${top_srcdir}/libgnucash/core-utils \
  -I${top_srcdir}/libgnucash/engine \
  -I${top_srcdir}/libgnucash/gnc-module \
  -I${top_srcdir}/libgnucash/app-utils \
  -I${top_srcdir}/gnucash/gnome \
  -I${top_srcdir}/gnucash/gnome-utils \
  -I${top_srcdir}/gnucash/import-export \
  ${GUILE_CFLAGS} \
  ${GLIB_CFLAGS} \
  ${GTK_CFLAGS}

uidir = $(GNC_UI_DIR)
ui_DATA = \
	gnc-plugin-qif-import-ui.xml

EXTRA_DIST = \
  ${gncscm_DATA} \
  ${gtkbuilder_DATA} \
  file-format.txt \
  $(ui_DATA) \
  CMakeLists.txt

CLEANFILES =

if GNUCASH_SEPARATE_BUILDDIR
SCM_FILE_LINKS = \
  ${gncscm_DATA}
endif

noinst_DATA = .scm-links

.scm-links: 
	$(RM) -rf gnucash
	mkdir -p  gnucash qif-import
	mkdir -p  gnucash/import-export
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	( cd gnucash/import-export; for A in $(gncscmmod_DATA) ; do $(LN_S) -f ../../$$A . ; done )
	( cd qif-import; for A in $(gncscm_DATA) ; do $(LN_S) -f ../$$A . ; done )
if ! OS_WIN32
# Windows knows no "ln -s" but uses "cp": must copy every time (see bug #566567).
	touch .scm-links
endif

if GNC_HAVE_GUILE_2
GUILE_COMPILE_ENV = \
  --gnc-module-dir ${top_builddir}/libgnucash/engine \
  --guile-load-dir ${top_builddir}/libgnucash/app-utils \
  --guile-load-dir ${top_builddir}/libgnucash/core-utils \
  --guile-load-dir ${top_builddir}/libgnucash/engine \
  --guile-load-dir ${top_builddir}/libgnucash/gnc-module \
  --guile-load-dir ${top_builddir}/libgnucash/scm \
  --library-dir    ${top_builddir}/libgnucash/engine \
  --library-dir    ${top_builddir}/libgnucash/core-utils \
  --library-dir    ${top_builddir}/libgnucash/app-utils \
  --library-dir    ${top_builddir}/gnucash/gnome \
  --library-dir    ${top_builddir}/libgnucash/gnc-module \
  --library-dir    ${top_builddir}/gnucash/gnome-utils \
  --library-dir    ${top_builddir}/libgnucash/backend/xml \
  --library-dir    ${top_builddir}/libgnucash/backend/sql \
  --library-dir    ${top_builddir}/gnucash/html \
  --library-dir    ${top_builddir}/gnucash/report/report-system \
  --library-dir    ${top_builddir}/gnucash/report/report-gnome \
  --library-dir    ${top_builddir}/gnucash/register/register-gnome \
  --library-dir    ${top_builddir}/gnucash/register/register-core \
  --library-dir    ${top_builddir}/gnucash/register/ledger-core \
  --library-dir    ${top_builddir}/gnucash/gnome-search


%.go : %.scm .scm-links $(pkglib_LTLIBRARIES)
	GNC_UNINSTALLED=yes \
	GNC_BUILDDIR=${top_builddir} \
	$(shell ${abs_top_srcdir}/common/gnc-test-env.pl --noexports ${GUILE_COMPILE_ENV}) \
	$(GUILD) compile -o $@ $<

gncscmmodcachedir = ${pkglibdir}/scm/ccache/@GUILE_EFFECTIVE_VERSION@/gnucash/import-export
gncscmmodcache_DATA = $(gncscmmod_DATA:.scm=.go)

gncscmcachedir = ${pkglibdir}/scm/ccache/@GUILE_EFFECTIVE_VERSION@/qif-import
gncscmcache_DATA = $(gncscm_DATA:.scm=.go)
endif

clean-local:
	$(RM) -rf gnucash qif-import

CLEANFILES += .scm-links ${gncscmmodcache_DATA} ${gncscmcache_DATA}
DISTCLEANFILES = ${SCM_FILE_LINKS}

AM_CPPFLAGS += -DG_LOG_DOMAIN=\"gnc.import.qif.import\"
