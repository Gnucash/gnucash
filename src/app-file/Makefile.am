SUBDIRS = gnome . test

pkglib_LTLIBRARIES = libgncmod-app-file.la libgw-app-file.la

AM_CFLAGS = \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/gnome-utils \
  ${GNOME_INCLUDEDIR} \
  ${GUILE_INCS} \
  ${GLIB_CFLAGS} \
  ${G_WRAP_COMPILE_ARGS}

libgncmod_app_file_la_SOURCES = \
  gnc-file.c \
  gncmod-app-file.c

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  gnc-file.h \
  gnc-file-dialog.h \
  gnc-file-history.h

noinst_HEADERS = gnc-file-p.h

libgncmod_app_file_la_LDFLAGS = -module

libgncmod_app_file_la_LIBADD = \
  ../gnc-module/libgncmodule.la \
  ../engine/libgncmod-engine.la \
  ../gnome-utils/libgncmod-gnome-utils.la \
  ./gnome/libgnc-app-file-gnome.la \
  ${GUILE_LIBS} \
  ${GLIB_LIBS}

libgw_app_file_la_SOURCES = gw-app-file.c
libgw_app_file_la_LDFLAGS = -module

gwmoddir = ${GNC_GWRAP_LIBDIR}
gwmod_DATA = gw-app-file-spec.scm gw-app-file.scm

noinst_DATA = .scm-links

EXTRA_DIST = \
  ${gwmod_DATA}

if GNUCASH_SEPARATE_BUILDDIR
#Only needed when srcdir and builddir are different
SCM_FILE_LINKS = gw-app-file-spec.scm
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

gw-app-file.scm gw-app-file.c gw-app-file.h: .scm-links gw-app-file-spec.scm
	FLAVOR=gnome guile -c \
	 "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	  (set! %load-path (cons \"${PWD}\" %load-path)) \
	  (set! %load-path (cons \"${top_builddir}/src/engine\" %load-path)) \
	  (primitive-load \"./gw-app-file-spec.scm\") \
	  (gw:generate-wrapset \"gw-app-file\")"

BUILT_SOURCES = gw-app-file.scm gw-app-file.c gw-app-file.h
CLEANFILES = ${BUILT_SOURCES} gw-app-file.html \
             ${SCM_FILE_LINKS} gnucash g-wrapped .scm-links
