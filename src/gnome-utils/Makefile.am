SUBDIRS = schemas . test ui
PWD := $(shell pwd)

pkglib_LTLIBRARIES = libgncmod-gnome-utils.la libgw-gnome-utils.la

# Note that src/gnome-utils CANNOT depend on src/gnome!

AM_CFLAGS = \
  -DPREFIX=\"${prefix}\" \
  -DSYSCONFDIR=\"${GNC_CONFIGDIR}\" \
  -DDATADIR=\"${GNC_SHAREDIR}\" \
  -DLIBDIR=\"${GNC_LIBDIR}\" \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/backend/file \
  -I${top_srcdir}/src/network-utils \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src \
  -I${top_srcdir}/lib/glib26 \
  ${GLIB_CFLAGS} \
  ${GLADE_CFLAGS} \
  ${GNOME_PRINT_CFLAGS} \
  ${GNOME_CFLAGS} \
  ${GTKHTML_CFLAGS} \
  ${GUILE_INCS} \
  ${G_WRAP_COMPILE_ARGS} \
  ${QOF_CFLAGS} \
  ${GOFFICE_CFLAGS} \
  ${GSF_CFLAGS}

libgncmod_gnome_utils_la_SOURCES = \
  QuickFill.c \
  account-quickfill.c \
  cursors.c \
  argv-list-converters.c \
  dialog-account.c \
  dialog-commodity.c \
  dialog-options.c \
  dialog-preferences.c \
  dialog-query-list.c \
  dialog-reset-warnings.c \
  dialog-transfer.c \
  dialog-utils.c \
  druid-utils.c \
  druid-gconf-setup.c \
  gnc-account-sel.c \
  gnc-amount-edit.c \
  gnc-commodity-edit.c \
  gnc-currency-edit.c \
  gnc-date-delta.c \
  gnc-date-edit.c \
  gnc-date-format.c \
  gnc-dense-cal.c \
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
  gnc-html-graph-gog.c \
  gnc-html-history.c \
  gnc-html.c \
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
  gnc-tree-model.c \
  gnc-tree-model-account-types.c \
  gnc-tree-model-account.c \
  gnc-tree-model-commodity.c \
  gnc-tree-model-example-account.c \
  gnc-tree-model-price.c \
  gnc-tree-model-selection.c \
  gnc-tree-view-account.c \
  gnc-tree-view-commodity.c \
  gnc-tree-view-price.c \
  gnc-tree-view.c \
  gnc-window.c \
  gncmod-gnome-utils.c \
  misc-gnome-utils.c \
  print-session.c \
  search-param.c \
  gnc-dialog.c \
  gnc-tree-model-budget.c

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  QuickFill.h \
  account-quickfill.h \
  dialog-account.h \
  dialog-commodity.h \
  dialog-preferences.h \
  dialog-options.h \
  dialog-query-list.h \
  dialog-reset-warnings.h \
  dialog-transfer.h \
  dialog-utils.h \
  druid-utils.h \
  druid-gconf-setup.h \
  gnc-account-sel.h \
  gnc-amount-edit.h \
  gnc-commodity-edit.h \
  gnc-currency-edit.h \
  gnc-date-delta.h \
  gnc-date-edit.h \
  gnc-date-format.h \
  gnc-dense-cal.h \
  gnc-druid-gnome-ui.h \
  gnc-embedded-window.h \
  gnc-file.h \
  gnc-frequency.h \
  gnc-recurrence.h \
  gnc-general-select.h \
  gnc-gnome-utils.h \
  gnc-gui-query.h \
  gnc-html-graph-gog.h \
  gnc-html-history.h \
  gnc-html.h \
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
  gnc-tree-model.h \
  gnc-tree-model-account-types.h \
  gnc-tree-model-account.h \
  gnc-tree-model-commodity.h \
  gnc-tree-model-example-account.h \
  gnc-tree-model-price.h \
  gnc-tree-model-selection.h \
  gnc-tree-view-account.h \
  gnc-tree-view-commodity.h \
  gnc-tree-view-price.h \
  gnc-tree-view.h \
  gnc-window.h \
  misc-gnome-utils.h \
  print-session.h \
  gnc-dialog.h \
  gnc-tree-model-budget.h

noinst_HEADERS = \
  argv-list-converters.h \
  gnc-dir.h \
  gnc-druid-gnome.h \
  gnc-druid-provider-edge-gnome.h \
  gnc-druid-provider-file-gnome.h \
  gnc-druid-provider-multifile-gnome.h \
  gnc-version.h \
  gw-gnome-utils.h \
  search-param.h

libgncmod_gnome_utils_la_LIBADD = \
  ${top_builddir}/src/core-utils/libcore-utils.la \
  ${top_builddir}/src/gnc-module/libgncmodule.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/calculation/libgncmod-calculation.la \
  ${top_builddir}/src/network-utils/libgncmod-network-utils.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/lib/glib26/libgncglib.la \
  ${GNOME_LIBS} \
  ${GNOME_PRINT_LIBS} \
  ${GTKHTML_LIBS} \
  ${GUILE_LIBS} \
  ${GDK_PIXBUF_LIBS} \
  ${GLADE_LIBS} \
  ${GUILE_LIBS} \
  ${GLIB_LIBS} \
  ${DB_LIBS} \
  ${GSF_LIBS} \
  ${QOF_LIBS} \
  ${GOFFICE_LIBS} \
  ${XML_LIBS}

libgw_gnome_utils_la_SOURCES = gw-gnome-utils.c

gwmoddir = ${GNC_GWRAP_LIBDIR}
gwmod_DATA = gw-gnome-utils.scm gw-gnome-utils-spec.scm

gncmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash
gncmod_DATA = gnome-utils.scm 

gncscmdir = ${GNC_SHAREDIR}/scm
gncscm_DATA = gnc-menu-extensions.scm

gladedir = $(GNC_GLADE_DIR)
glade_DATA = \
  commodity.glade \
  dialog-query-list.glade \
  dialog-reset-warnings.glade \
  druid-provider-multifile.glade \
  exchange-dialog.glade \
  druid-gconf-setup.glade \
  gnc-date-format.glade \
  gnc-gui-query.glade \
  preferences.glade \
  transfer.glade

EXTRA_DIST = \
  gnc-dir.h.in \
  gnc-svninfo.h \
  ${glade_DATA} \
  ${gwmod_DATA} \
  ${gncmod_DATA} \
  ${gncscm_DATA}

## We borrow guile's convention and use @-...-@ as the substitution
## brackets here, instead of the usual @...@.  This prevents autoconf
## from substituting the values directly into the left-hand sides of
## the sed substitutions.
gnc-dir.h: gnc-dir.h.in ${top_builddir}/config.status
	rm -f $@.tmp
	sed < $< > $@.tmp \
	    -e 's:@-GNC_ACCOUNTS_DIR-@:${GNC_ACCOUNTS_DIR}:g' \
	    -e 's:@-GNC_GLADE_DIR-@:${GNC_GLADE_DIR}:g' \
	    -e 's:@-GCONF_SCHEMA_CONFIG_SOURCE_DIRONLY-@:${GCONF_SCHEMA_CONFIG_SOURCE_DIRONLY}:g'
	mv $@.tmp $@

gnc-version.h: _gnc-version.h
	-[ ! -f gnc-version.h ] && cp _gnc-version.h gnc-version.h
	-cmp -s _gnc-version.h gnc-version.h || cp _gnc-version.h gnc-version.h
	-rm -f _gnc-version.h

_gnc-version.h: gnc-svninfo.h
	@echo "/* Autogenerated. Do not change. */"	 > _gnc-version.h
	@echo "#ifndef GNC_VERSION_H"			>> _gnc-version.h
	@echo "#define GNC_VERSION_H"			>> _gnc-version.h
	@echo ""					>> _gnc-version.h
	@s=${srcdir} ; \
	if [ -h ${srcdir}/Makefile.am ] ; then \
		d=`readlink $(srcdir)/Makefile.am` ; \
		s="${srcdir}/`dirname $$d`" ; \
	fi ; \
	if [ -d $$s/.svn ] ; then \
		echo "#define GNUCASH_SVN 1"		>> _gnc-version.h ; \
	fi
	@echo "#define GNUCASH_BUILD_DATE \"`date +%Y-%m-%d`\"" >> _gnc-version.h
	@echo "#include \"gnc-svninfo.h\""		>> _gnc-version.h
	@echo "#endif"					>> _gnc-version.h

gnc-svninfo.h: .gnc-svninfo.h
	@s=${srcdir} ; \
	if [ -h ${srcdir}/Makefile.am ] ; then \
		d=`readlink $(srcdir)/Makefile.am` ; \
		s="${srcdir}/`dirname $$d`" ; \
	fi ; \
	if [ -d $$s/.svn ] ; then \
		svninfo=`LANG=C svn info $$s |grep '^Revision: '|sed 's/Revision: //'` ; \
		if [ -z "$$svninfo" ] ; then \
			echo "svn info failed.  figure out why." ; \
			echo "can't determine svn revision from $$s." ; \
			exit 1 ; \
		fi ; \
		echo "#define GNUCASH_SVN_REV $$svninfo" > gnc-svninfo.h ; \
	fi

.gnc-svninfo.h:

BUILT_SOURCES = gnc-dir.h gnc-version.h

if GNUCASH_SEPARATE_BUILDDIR
#For compiling
SCM_FILE_LINKS = gw-gnome-utils-spec.scm
#For executing test cases
SCM_FILE_LINKS += gnome-utils-spec.scm gnome-utils.scm
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

gw-gnome-utils.scm gw-gnome-utils.c gw-gnome-utils.h: \
  .scm-links gw-gnome-utils-spec.scm ${top_builddir}/config.status
	FLAVOR=gnome $(GUILE) -c \
	 "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	  (set! %load-path (cons \"${PWD}\" %load-path)) \
	  (set! %load-path (cons \"${top_builddir}/src/engine\" %load-path)) \
	  (set! %load-path (cons \"${top_builddir}/src/app-utils\" %load-path)) \
	  (primitive-load \"./gw-gnome-utils-spec.scm\") \
	  (gw:generate-wrapset \"gw-gnome-utils\")"

BUILT_SOURCES += gw-gnome-utils.scm gw-gnome-utils.c gw-gnome-utils.h
DISTCLEANFILES = gnucash g-wrapped .scm-links ${SCM_FILE_LINKS} \
                 gw-gnome-utils.html gnc-svninfo.h

#
# I hate inconsistent standards. Autotools puts help files into
# ${datadir}/gnome/help/${program} while the gnome2 libraries expect
# them in ${pkgdatadir}/gnome/help/${program}.
#
install-data-hook:
	ln -sf ../gnome ${DESTDIR}/${pkgdatadir}

uninstall-hook:
	rm -f ${DESTDIR}/${pkgdatadir}/gnome
