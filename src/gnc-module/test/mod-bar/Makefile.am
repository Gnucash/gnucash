AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnc-module \
  ${G_WRAP_COMPILE_ARGS} \
  ${GUILE_INCS} \
  ${GLIB_CFLAGS}

checklibdir=${PWD}
checklib_LTLIBRARIES = libbar.la libgncmodbar.la

libbar_la_LDFLAGS = -module
libbar_la_SOURCES = bar.c

noinst_HEADERS = \
  bar.h \
  bar-gwrap.h

libgncmodbar_la_SOURCES=bar-gwrap.c gnc-mod-bar.c
libgncmodbar_la_LDFLAGS=-module
libgncmodbar_la_LIBADD=libbar.la

EXTRA_DIST = \
  bar-gwrap.scm \
  gnucash/bar.scm

bar-gwrap.c bar-gwrap.h: bar-gwrap.scm ${top_builddir}/config.status
	FLAVOR=gnome guile -c \
	  "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	  (primitive-load \"${srcdir}/bar-gwrap.scm\") \
	  (gw:generate-wrapset \"bar-gwrap\")"

BUILT_SOURCES = bar-gwrap.c bar-gwrap.h

DISTCLEANFILES = bar-gwrap.html

# Keep automake from generating install targets for test libs
install-checklibLTLIBRARIES:
uninstall-checklibLTLIBRARIES:
