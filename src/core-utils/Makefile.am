
lib_LTLIBRARIES = libcore-utils.la libgw-core-utils.la

libcore_utils_la_SOURCES = \
  gnc-gconf-utils.c \
  gnc-gdate-utils.c \
  gnc-gkeyfile-utils.c \
  gnc-gobject-utils.c

libcore_utils_la_LIBADD = \
  ${top_builddir}/lib/glib26/libgncglib.la \
  ${GLIB_LIBS} \
  ${GCONF_LIBS} \
  ${GTK_LIBS}

libgw_core_utils_la_SOURCES = \
  gw-core-utils.c

libgw_core_utils_la_LDFLAGS = ${G_WRAP_LINK_ARGS}
libgw_core_utils_la_LIBADD = \
  libcore-utils.la \
  ${GUILE_LIBS}

noinst_HEADERS = \
  gnc-gconf-utils.h \
  gnc-gdate-utils.h \
  gnc-gkeyfile-utils.h \
  gnc-gobject-utils.h \
  gw-core-utils.h

EXTRA_DIST = ${gwmod_DATA}

AM_CFLAGS = \
  -I${top_srcdir}/lib/glib26 \
  ${G_WRAP_COMPILE_ARGS} \
  ${GUILE_INCS} \
  ${GLIB_CFLAGS} \
  ${GCONF_CFLAGS} \
  ${GTK_CFLAGS}

gwmoddir = ${GNC_GWRAP_LIBDIR}
gwmod_DATA = gw-core-utils-spec.scm gw-core-utils.scm

if GNUCASH_SEPARATE_BUILDDIR
#Only needed when srcdir and builddir are different
SCM_FILE_LINKS = gw-core-utils-spec.scm
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

gw-core-utils.scm gw-core-utils.h gw-core-utils.c gw-core-utils.html: \
  gw-core-utils-spec.scm .scm-links ${top_builddir}/config.status
	FLAVOR=gnome guile -c \
          "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
           (primitive-load \"./gw-core-utils-spec.scm\") \
           (gw:generate-wrapset \"gw-core-utils\")"

BUILT_SOURCES = gw-core-utils.scm gw-core-utils.h gw-core-utils.c
DISTCLEANFILES = g-wrapped gnucash .scm-links ${SCM_FILE_LINKS} \
                 gw-core-utils.html
