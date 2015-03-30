SUBDIRS = gtkbuilder gschemas . test ui

pkglib_LTLIBRARIES = libgncmod-gnome-utils.la

# Note that src/gnome-utils CANNOT depend on src/gnome!

AM_CPPFLAGS = \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/backend/xml \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src \
  -I${top_builddir}/src \
  -I${top_srcdir}/lib/libc \
  -I${top_srcdir}/src/libqof/qof \
  ${GLIB_CFLAGS} \
  ${GTK_CFLAGS} \
  ${GNOME_KEYRING_CFLAGS} \
  ${LIBSECRET_CFLAGS} \
  ${GUILE_CFLAGS} \
  ${QOF_CFLAGS} \
  ${LIBGDA_CFLAGS} \
  ${LIBXML2_CFLAGS} \
  ${GTK_MAC_CFLAGS}

libgncmod_gnome_utils_la_SOURCES = \
  account-quickfill.c \
  assistant-utils.c \
  assistant-xml-encoding.c \
  cursors.c \
  dialog-account.c \
  dialog-book-close.c \
  dialog-commodity.c \
  dialog-dup-trans.c \
  dialog-file-access.c \
  dialog-object-references.c \
  dialog-options.c \
  dialog-preferences.c \
  dialog-query-view.c \
  dialog-reset-warnings.c \
  dialog-tax-table.c \
  dialog-totd.c \
  dialog-transfer.c \
  dialog-userpass.c \
  dialog-utils.c \
  gnc-account-sel.c \
  gnc-amount-edit.c \
  gnc-autosave.c \
  gnc-cell-renderer-date.c \
  gnc-cell-renderer-popup.c \
  gnc-cell-renderer-popup-entry.c \
  gnc-combott.c \
  gnc-commodity-edit.c \
  gnc-currency-edit.c \
  gnc-date-delta.c \
  gnc-date-edit.c \
  gnc-date-format.c \
  gnc-dense-cal.c \
  gnc-dense-cal-model.c \
  gnc-dense-cal-store.c \
  gnc-embedded-window.c \
  gnc-file.c \
  gnc-frequency.c \
  gnc-recurrence.c \
  gnc-general-select.c \
  gnc-gnome-utils.c \
  gnc-gobject-utils.c \
  gnc-gtk-utils.c \
  gnc-gui-query.c \
  gnc-icons.c \
  gnc-keyring.c \
  gnc-main-window.c \
  gnc-menu-extensions.c \
  gnc-plugin-file-history.c \
  gnc-plugin-manager.c \
  gnc-plugin-menu-additions.c \
  gnc-plugin-page.c \
  gnc-plugin.c \
  gnc-period-select.c \
  gnc-query-view.c \
  gnc-splash.c \
  gnc-sx-instance-dense-cal-adapter.c \
  gnc-sx-list-tree-model-adapter.c \
  gnc-tree-control-split-reg.c \
  gnc-tree-model.c \
  gnc-tree-model-account-types.c \
  gnc-tree-model-account.c \
  gnc-tree-model-budget.c \
  gnc-tree-model-owner.c \
  gnc-tree-model-commodity.c \
  gnc-tree-model-price.c \
  gnc-tree-model-split-reg.c \
  gnc-tree-util-split-reg.c \
  gnc-tree-view-account.c \
  gnc-tree-view-commodity.c \
  gnc-tree-view-owner.c \
  gnc-tree-view-price.c \
  gnc-tree-view-split-reg.c \
  gnc-tree-view-sx-list.c \
  gnc-tree-view.c \
  gnc-warnings.c \
  gnc-window.c \
  gncmod-gnome-utils.c \
  misc-gnome-utils.c \
  tree-view-utils.c \
  search-param.c \
  print-session.c \
  swig-gnome-utils.c \
  window-main-summarybar.c

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  account-quickfill.h \
  assistant-utils.h \
  assistant-xml-encoding.h \
  dialog-account.h \
  dialog-book-close.h \
  dialog-commodity.h \
  dialog-dup-trans.h \
  dialog-file-access.h \
  dialog-preferences.h \
  dialog-object-references.h \
  dialog-options.h \
  dialog-query-view.h \
  dialog-reset-warnings.h \
  dialog-totd.h \
  dialog-transfer.h \
  dialog-utils.h \
  gnc-account-sel.h \
  gnc-amount-edit.h \
  gnc-cell-renderer-date.h \
  gnc-cell-renderer-popup.h \
  gnc-cell-renderer-popup-entry.h \
  gnc-combott.h \
  gnc-commodity-edit.h \
  gnc-currency-edit.h \
  gnc-date-delta.h \
  gnc-date-edit.h \
  gnc-date-format.h \
  gnc-dense-cal.h \
  gnc-dense-cal-model.h \
  gnc-dense-cal-store.h \
  gnc-embedded-window.h \
  gnc-file.h \
  gnc-frequency.h \
  gnc-recurrence.h \
  gnc-general-select.h \
  gnc-gnome-utils.h \
  gnc-gui-query.h \
  gnc-icons.h \
  gnc-keyring.h \
  gnc-main-window.h \
  gnc-menu-extensions.h \
  gnc-plugin-file-history.h \
  gnc-plugin-manager.h \
  gnc-plugin-menu-additions.h \
  gnc-plugin-page.h \
  gnc-plugin.h \
  gnc-period-select.h \
  gnc-query-view.h \
  gnc-splash.h \
  gnc-sx-instance-dense-cal-adapter.h \
  gnc-sx-list-tree-model-adapter.h \
  gnc-tree-control-split-reg.h \
  gnc-tree-model.h \
  gnc-tree-model-account-types.h \
  gnc-tree-model-account.h \
  gnc-tree-model-budget.h \
  gnc-tree-model-owner.h \
  gnc-tree-model-commodity.h \
  gnc-tree-model-price.h \
  gnc-tree-model-split-reg.h \
  gnc-tree-util-split-reg.h \
  gnc-tree-view-account.h \
  gnc-tree-view-commodity.h \
  gnc-tree-view-owner.h \
  gnc-tree-view-price.h \
  gnc-tree-view-split-reg.h \
  gnc-tree-view-sx-list.h \
  gnc-tree-view.h \
  gnc-ui.h \
  gnc-window.h \
  misc-gnome-utils.h \
  tree-view-utils.h \
  print-session.h \
  window-main-summarybar.h

noinst_HEADERS = \
  dialog-tax-table.h \
  gnc-autosave.h \
  gnc-gobject-utils.h \
  gnc-gtk-utils.h \
  search-param.h

libgncmod_gnome_utils_la_LDFLAGS = -avoid-version

libgncmod_gnome_utils_la_LIBADD = \
  ${top_builddir}/src/core-utils/libgnc-core-utils.la \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/backend/xml/libgnc-backend-xml-utils.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  $(top_builddir)/lib/libc/libc-missing.la \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${GTK_LIBS} \
  ${GNOME_KEYRING_LIBS} \
  ${LIBSECRET_LIBS} \
  ${GUILE_LIBS} \
  ${GLIB_LIBS} \
  ${DB_LIBS} \
  ${REGEX_LIBS} \
  ${LIBXML2_LIBS} \
  ${GTK_MAC_LIBS}

BUILT_SOURCES = gnc-warnings.c gnc-warnings.h

gnc-warnings.c: gschemas/org.gnucash.warnings.gschema.xml.in make-gnc-warnings-c.xsl
	$(XSLTPROC) -o $@ $(srcdir)/make-gnc-warnings-c.xsl $<

gnc-warnings.h: gschemas/org.gnucash.warnings.gschema.xml.in make-gnc-warnings-h.xsl
	$(XSLTPROC) -o $@ $(srcdir)/make-gnc-warnings-h.xsl $<

if HAVE_X11_XLIB_H
  libgncmod_gnome_utils_la_LIBADD += -lX11
endif

if BUILDING_FROM_VCS
swig-gnome-utils.c: gnome-utils.i \
                    ${top_srcdir}/src/base-typemaps.i
	$(SWIG) -guile $(SWIG_ARGS) -Linkage module \
	-I${top_srcdir}/src -o $@ $<
if ! OS_WIN32
if ! SWIG_DIST_FAIL
	if ! `grep "define scm_from_utf8_string" $@ > /dev/null 2>&1`; then \
	  patch $@ $(top_srcdir)/src/swig-utf8.patch; \
	fi
endif
endif
endif

gncscmmoddir = ${GNC_SCM_INSTALL_DIR}/gnucash
gncscmmod_DATA = gnome-utils.scm

gncscmdir = ${GNC_SCM_INSTALL_DIR}
gncscm_DATA = gnc-menu-extensions.scm

EXTRA_DIST = \
  gnome-utils.i \
  make-gnc-warnings-c.xsl \
  make-gnc-warnings-h.xsl \
  ${gncscmmod_DATA} \
  ${gncscm_DATA}


if GNUCASH_SEPARATE_BUILDDIR
#For executing test cases
SCM_FILE_LINKS = \
  ${gncscmmod_DATA} \
  ${gncscm_DATA}
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
  --guile-load-dir ${top_builddir}/src/core-utils \
  --guile-load-dir ${top_builddir}/src/gnc-module \
  --guile-load-dir ${top_builddir}/src/scm \
  --library-dir    ${top_builddir}/src/engine \
  --library-dir    ${top_builddir}/src/libqof/qof \
  --library-dir    ${top_builddir}/src/core-utils \
  --library-dir    ${top_builddir}/src/gnc-module \
  --library-dir    ${top_builddir}/src/app-utils \
  --library-dir    ${top_builddir}/src/backend/xml \
  --library-dir    ${top_builddir}/src/gnome-utils

%.go : %.scm .scm-links $(pkglib_LTLIBRARIES)
	GNC_UNINSTALLED=yes \
	GNC_BUILDDIR=${top_builddir} \
	$(shell ${abs_top_srcdir}/src/gnc-test-env.pl --noexports ${GUILE_COMPILE_ENV}) \
	$(GUILD) compile -o $@ $<

gncscmmodcachedir = ${pkglibdir}/scm/ccache/@GUILE_EFFECTIVE_VERSION@/gnucash
gncscmmodcache_DATA = $(gncscmmod_DATA:.scm=.go)

gncscmcachedir = ${pkglibdir}/scm/ccache/@GUILE_EFFECTIVE_VERSION@
gncscmcache_DATA = $(gncscm_DATA:.scm=.go)
endif

noinst_DATA = .scm-links
clean-local:
	rm -rf gnucash

CLEANFILES = \
	$(BUILT_SOURCES) \
	.scm-links \
	${SCM_FILE_LINKS} \
	${gncscmcache_DATA} \
	${gncscmmodcache_DATA}

MAINTAINERCLEANFILES = swig-gnome-utils.c

# FIXME: Symlinking directories only works on non-win32.
if !PLATFORM_WIN32
#
# I hate inconsistent standards. Autotools puts help files into
# ${datadir}/gnome/help/${program} while the gnome2 libraries expect
# them in ${pkgdatadir}/gnome/help/${program}.
#
install-data-hook:
	$(LN_S) -f ../gnome ${DESTDIR}${pkgdatadir}

uninstall-hook:
	rm -f ${DESTDIR}${pkgdatadir}/gnome
endif

AM_CPPFLAGS += -DG_LOG_DOMAIN=\"gnc.gui\"
