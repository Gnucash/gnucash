
lib_LTLIBRARIES = libcore-utils.la libgw-core-utils.la

gwmoddir = ${GNC_GWRAP_LIBDIR}
gwmod_DATA = gw-core-utils-spec.scm gw-core-utils.scm

libcore_utils_la_SOURCES = core-utils.c
libcore_utils_la_LDFLAGS = -module ${GLIB_LIBS}
libcore_utils_la_LIBADD =

libgw_core_utils_la_SOURCES = gw-core-utils.c
libgw_core_utils_la_LDFLAGS = -module ${G_WRAP_LINK_ARGS} ${GUILE_LIBS}
libgw_core_utils_la_LIBADD = libcore-utils.la

noinst_HEADERS = core-utils.h gw-core-utils.h

EXTRA_DIST = .cvsignore

AM_CFLAGS = ${G_WRAP_COMPILE_ARGS} ${GLIB_CFLAGS} ${GUILE_INCS}

.scm-links: 
	rm -f gnucash g-wrapped
	ln -sf . gnucash 
	ln -sf . g-wrapped 
	touch .scm-links
DISTCLEANFILES = .scm-links gnucash g-wrapped

gw-core-utils.scm gw-core-utils.h gw-core-utils.c gw-core-utils.html: \
  gw-core-utils-spec.scm .scm-links
	FLAVOR=gnome guile -c \
          "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
           (primitive-load \"./gw-core-utils-spec.scm\") \
           (gw:generate-wrapset \"gw-core-utils\")"
BUILT_SOURCES = gw-core-utils.scm gw-core-utils.h gw-core-utils.c
CLEANFILES = gw-core-utils.scm gw-core-utils.h gw-core-utils.c 
