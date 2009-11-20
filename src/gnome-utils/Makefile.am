SUBDIRS = glade schemas . test ui

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
  ${GLADE_CFLAGS} \
  ${GTK_CFLAGS} \
  ${GNOME_CFLAGS} \
  ${GNOME_PRINT_CFLAGS} \
  ${GUILE_INCS} \
  ${QOF_CFLAGS} \
  ${GOFFICE_CFLAGS} \
  ${LIBGDA_CFLAGS} \
  ${IGE_MAC_CFLAGS}

libgncmod_gnome_utils_la_SOURCES = \
  QuickFill.c \
  account-quickfill.c \
  cursors.c \
  dialog-account.c \
  dialog-book-close.c \
  dialog-commodity.c \
  dialog-file-access.c \
  dialog-options.c \
  dialog-preferences.c \
  dialog-query-list.c \
  dialog-reset-warnings.c \
  dialog-totd.c \
  dialog-transfer.c \
  dialog-utils.c \
  druid-utils.c \
  druid-gconf-setup.c \
  druid-gnc-xml-import.c \
  gnc-account-sel.c \
  gnc-amount-edit.c \
  gnc-autosave.c \
  gnc-commodity-edit.c \
  gnc-currency-edit.c \
  gnc-date-delta.c \
  gnc-date-edit.c \
  gnc-date-format.c \
  gnc-dense-cal.c \
  gnc-dense-cal-model.c \
  gnc-dense-cal-store.c \
  gnc-dialog.c \
  gnc-druid-gnome.c \
  gnc-druid-provider-edge-gnome.c \
  gnc-druid-provider-file-gnome.c \
  gnc-druid-provider-multifile-gnome.c \
  gnc-embedded-window.c \
  gnc-file.c \
  gnc-frequency.c \
  gnc-recurrence.c \
  gnc-general-select.c \
  gnc-gnome-utils.c \
  gnc-gui-query.c \
  gnc-icons.c \
  gnc-main-window.c \
  gnc-menu-extensions.c \
  gnc-plugin-file-history.c \
  gnc-plugin-manager.c \
  gnc-plugin-menu-additions.c \
  gnc-plugin-page.c \
  gnc-plugin.c \
  gnc-period-select.c \
  gnc-query-list.c \
  gnc-splash.c \
  gnc-sx-instance-dense-cal-adapter.c \
  gnc-sx-list-tree-model-adapter.c \
  gnc-tree-model.c \
  gnc-tree-model-account-types.c \
  gnc-tree-model-account.c \
  gnc-tree-model-account-drag.c \
  gnc-tree-model-budget.c \
  gnc-tree-model-commodity.c \
  gnc-tree-model-price.c \
  gnc-tree-view-account.c \
  gnc-tree-view-commodity.c \
  gnc-tree-view-price.c \
  gnc-tree-view-sx-list.c \
  gnc-tree-view.c \
  gnc-window.c \
  gncmod-gnome-utils.c \
  misc-gnome-utils.c \
  search-param.c \
  print-session.c \
  swig-gnome-utils.c \
  window-main-summarybar.c

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  QuickFill.h \
  account-quickfill.h \
  dialog-account.h \
  dialog-book-close.h \
  dialog-commodity.h \
  dialog-file-access.h \
  dialog-preferences.h \
  dialog-options.h \
  dialog-query-list.h \
  dialog-reset-warnings.h \
  dialog-totd.h \
  dialog-transfer.h \
  dialog-utils.h \
  druid-utils.h \
  druid-gconf-setup.h \
  druid-gnc-xml-import.h \
  gnc-account-sel.h \
  gnc-amount-edit.h \
  gnc-commodity-edit.h \
  gnc-currency-edit.h \
  gnc-date-delta.h \
  gnc-date-edit.h \
  gnc-date-format.h \
  gnc-dense-cal.h \
  gnc-dense-cal-model.h \
  gnc-dense-cal-store.h \
  gnc-druid-gnome-ui.h \
  gnc-embedded-window.h \
  gnc-file.h \
  gnc-frequency.h \
  gnc-recurrence.h \
  gnc-general-select.h \
  gnc-gnome-utils.h \
  gnc-gui-query.h \
  gnc-icons.h \
  gnc-main-window.h \
  gnc-menu-extensions.h \
  gnc-plugin-file-history.h \
  gnc-plugin-manager.h \
  gnc-plugin-menu-additions.h \
  gnc-plugin-page.h \
  gnc-plugin.h \
  gnc-period-select.h \
  gnc-query-list.h \
  gnc-splash.h \
  gnc-sx-instance-dense-cal-adapter.h \
  gnc-sx-list-tree-model-adapter.h \
  gnc-tree-model.h \
  gnc-tree-model-account-types.h \
  gnc-tree-model-account.h \
  gnc-tree-model-account-drag.h \
  gnc-tree-model-budget.h \
  gnc-tree-model-commodity.h \
  gnc-tree-model-price.h \
  gnc-tree-view-account.h \
  gnc-tree-view-commodity.h \
  gnc-tree-view-price.h \
  gnc-tree-view-sx-list.h \
  gnc-tree-view.h \
  gnc-window.h \
  misc-gnome-utils.h \
  print-session.h \
  gnc-dialog.h \
  window-main-summarybar.h

noinst_HEADERS = \
  gnc-autosave.h \
  gnc-druid-gnome.h \
  gnc-druid-provider-edge-gnome.h \
  gnc-druid-provider-file-gnome.h \
  gnc-druid-provider-multifile-gnome.h \
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
  ${GNOME_LIBS} \
  ${GNOME_PRINT_LIBS} \
  ${GLADE_LIBS} \
  ${GUILE_LIBS} \
  ${GLIB_LIBS} \
  ${DB_LIBS} \
  ${REGEX_LIBS} \
  ${LIBXML2_LIBS} \
  ${IGE_MAC_LIBS}

if HAVE_X11_XLIB_H
  libgncmod_gnome_utils_la_LIBADD += -lX11
endif

if BUILDING_FROM_SVN
swig-gnome-utils.c: gnome-utils.i \
                    ${top_srcdir}/src/base-typemaps.i
	$(SWIG) -guile $(SWIG_ARGS) -Linkage module \
	-I${top_srcdir}/src -o $@ $<
endif

gncmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash
gncmod_DATA = gnome-utils.scm 

gncscmdir = ${GNC_SHAREDIR}/scm
gncscm_DATA = gnc-menu-extensions.scm

EXTRA_DIST = \
  gnc-svninfo.h \
  gnome-utils.i \
  ${gncmod_DATA} \
  ${gncscm_DATA}

## We borrow guile's convention and use @-...-@ as the substitution
## brackets here, instead of the usual @...@.  This prevents autoconf
## from substituting the values directly into the left-hand sides of
## the sed substitutions.
gnc-version.h: _gnc-version.h
	-if [ ! -f gnc-version.h ]; then cp _gnc-version.h gnc-version.h; fi
	-cmp -s _gnc-version.h gnc-version.h || cp _gnc-version.h gnc-version.h
	-rm -f _gnc-version.h

_gnc-version.h: gnc-svninfo.h Makefile
	@echo "/* Autogenerated. Do not change. */"	 > _gnc-version.h
	@echo "#ifndef GNC_VERSION_H"			>> _gnc-version.h
	@echo "#define GNC_VERSION_H"			>> _gnc-version.h
	@echo ""					>> _gnc-version.h
	@svninfo=`${top_srcdir}/util/gnc-svnversion ${top_srcdir}` ; \
	if [ $$? = 0 ] ; then \
		echo "#define GNUCASH_SVN 1"		>> _gnc-version.h ; \
	fi
	@echo "#define GNUCASH_BUILD_DATE \"`date +%Y-%m-%d`\"" >> _gnc-version.h
	@echo "#include \"gnc-svninfo.h\""		>> _gnc-version.h
	@echo "#endif"					>> _gnc-version.h

gnc-svninfo.h: _gnc-svninfo.h
	-if [ ! -f gnc-svninfo.h ]; then cp _gnc-svninfo.h gnc-svninfo.h; fi
	-cmp -s _gnc-svninfo.h gnc-svninfo.h || cp _gnc-svninfo.h gnc-svninfo.h
	-rm -f _gnc-svninfo.h

_gnc-svninfo.h: Makefile
	@svninfo=`${top_srcdir}/util/gnc-svnversion ${top_srcdir}` ; \
	if [ $$? = 0 ] ; then \
		if [ -z "$$svninfo" ] ; then \
			echo "gnc-svnversion failed.  figure out why." ; \
			echo "can't determine svn revision from ${top_srcdir}." ; \
			exit 1 ; \
		fi ; \
		echo "/* Autogenerated. Do not change. */" > _gnc-svninfo.h ; \
		echo "#define GNUCASH_SVN_REV \"$$svninfo\"" >> _gnc-svninfo.h ; \
	else \
		if [ -r $(srcdir)/gnc-svninfo.h ] ; then \
			cp $(srcdir)/gnc-svninfo.h _gnc-svninfo.h ; \
		else \
			echo "You're building from SVN...  But your build system is broken" ; \
			echo "Don't do that.   Complain to your build-system creator." ; \
			exit 1 ; \
		fi ; \
	fi

BUILT_SOURCES = gnc-version.h

if GNUCASH_SEPARATE_BUILDDIR
#For executing test cases
SCM_FILE_LINKS = \
  ${gncmod_DATA} \
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
	( cd gnucash; for A in $(gncmod_DATA) ; do $(LN_S) -f ../$$A . ; done )
if ! OS_WIN32
# Windows knows no "ln -s" but uses "cp": must copy every time (see bug #566567).
	touch .scm-links
endif

noinst_DATA = .scm-links
CLEANFILES = $(BUILT_SOURCES) gnucash .scm-links ${SCM_FILE_LINKS}
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

INCLUDES = -DG_LOG_DOMAIN=\"gnc.gui\"
