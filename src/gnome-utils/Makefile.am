SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-gnome-utils.la libgw-gnome-utils.la

AM_CFLAGS = \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/network-utils \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src \
  ${GUILE_INCS} \
  ${GUPPI_CFLAGS} \
  ${GLIB_CFLAGS} \
  ${GLADE_CFLAGS} \
  ${GNOME_PRINT_CFLAGS} \
  ${GNOME_INCLUDEDIR} \
  ${GDK_PIXBUF_CFLAGS} \
  ${GTKHTML_CFLAGS} \
  ${G_WRAP_COMPILE_ARGS}

libgncmod_gnome_utils_la_SOURCES = \
  cursors.c \
  argv-list-converters.c \
  dialog-commodity.c \
  dialog-options.c \
  dialog-utils.c \
  druid-utils.c \
  gnc-account-tree.c \
  gnc-amount-edit.c \
  gnc-commodity-edit.c \
  gnc-currency-edit.c \
  gnc-date-delta.c \
  gnc-date-edit.c \
  gnc-dense-cal.c \
  gnc-frequency.c \
  gnc-general-select.c \
  gnc-gnome-utils.c \
  gnc-gui-query.c \
  gnc-html-history.c \
  gnc-html-guppi.c \
  gnc-html.c \
  gnc-mdi-utils.c \
  gnc-menu-extensions.c \
  gncmod-gnome-utils.c \
  gtkselect.c \
  print-session.c \
  window-help.c

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  dialog-commodity.h \
  dialog-options.h \
  dialog-utils.h \
  druid-utils.h \
  gnc-account-tree.h \
  gnc-amount-edit.h \
  gnc-commodity-edit.h \
  gnc-currency-edit.h \
  gnc-date-delta.h \
  gnc-date-edit.h \
  gnc-dense-cal.h \
  gnc-frequency.h \
  gnc-general-select.h \
  gnc-gnome-utils.h \
  gnc-gui-query.h \
  gnc-html-history.h \
  gnc-html-guppi.h \
  gnc-html.h \
  gnc-mdi-utils.h \
  gnc-menu-extensions.h \
  gtkselect.h \
  print-session.h \
  window-help.h

noinst_HEADERS = \
  argv-list-converters.h \
  gnc-dir.h

libgncmod_gnome_utils_la_LDFLAGS = -module

libgncmod_gnome_utils_la_LIBADD = \
  ${top_builddir}/src/gnc-module/libgncmodule.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/calculation/libgncmod-calculation.la \
  ${top_builddir}/src/network-utils/libgncmod-network-utils.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${GUILE_LIBS} \
  ${GUPPI_LIBS} \
  ${GNOMEUI_LIBS} \
  ${GNOME_LIBDIR} \
  ${GNOME_PRINT_LIBS} \
  ${GTKHTML_LIBS} \
  ${GDK_PIXBUF_LIBS} \
  ${GLADE_LIBS} \
  ${GUILE_LIBS} \
  ${GLIB_LIBS} \
  ${DB_LIBS}

libgw_gnome_utils_la_SOURCES = gw-gnome-utils.c
libgw_gnome_utils_la_LDFLAGS = -module

gwmoddir = ${GNC_GWRAP_LIBDIR}
gwmod_DATA = gw-gnome-utils.scm gw-gnome-utils-spec.scm

gncmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash
gncmod_DATA = gnome-utils.scm 

gncscmdir = ${GNC_SHAREDIR}/scm
gncscm_DATA = gnc-menu-extensions.scm

gladedir = $(GNC_GLADE_DIR)
glade_DATA = \
  commodity.glade

EXTRA_DIST = \
  .cvsignore \
  gnc-dir.h.in \
  ${glade_DATA} \
  ${gwmod_DATA} \
  ${gncmod_DATA} \
  ${gncscm_DATA}

dialog-utils.c: gnc-dir.h

## We borrow guile's convention and use @-...-@ as the substitution
## brackets here, instead of the usual @...@.  This prevents autoconf
## from substituting the values directly into the left-hand sides of
## the sed substitutions.
gnc-dir.h: gnc-dir.h.in
	rm -f $@.tmp
	sed < $< > $@.tmp \
	    -e 's:@-GNC_ACCOUNTS_DIR-@:${GNC_ACCOUNTS_DIR}:g' \
	    -e 's:@-GNC_GLADE_DIR-@:${GNC_GLADE_DIR}:g' \
	    -e 's:@-GNC_PIXMAP_DIR-@:${GNC_PIXMAP_DIR}:g'
	mv $@.tmp $@
BUILT_SOURCES = gnc-dir.h

# We have to do this because otherwise automake insists on putting
# these files into the dist tarfile.  If there's a a better way,
# by all means, let us know...
dist-hook:
	rm ${distdir}/gnc-dir.h

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

gw-gnome-utils.scm gw-gnome-utils.c gw-gnome-utils.h: .scm-links gw-gnome-utils-spec.scm
	FLAVOR=gnome guile -c \
	 "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	  (set! %load-path (cons \"${PWD}\" %load-path)) \
	  (set! %load-path (cons \"${top_builddir}/src/engine\" %load-path)) \
	  (set! %load-path (cons \"${top_builddir}/src/app-utils\" %load-path)) \
	  (primitive-load \"./gw-gnome-utils-spec.scm\") \
	  (gw:generate-wrapset \"gw-gnome-utils\")"

BUILT_SOURCES += gw-gnome-utils.scm gw-gnome-utils.c gw-gnome-utils.h
CLEANFILES = ${BUILT_SOURCES} gw-gnome-utils.html \
             ${SCM_FILE_LINKS} gnucash g-wrapped .scm-links
