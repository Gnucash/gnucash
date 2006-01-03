AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/gnc-module/test/mod-foo \
  ${GLIB_CFLAGS} \
  ${G_WRAP_COMPILE_ARGS} \
  ${GUILE_INCS}

PWD := $(shell pwd)
checklibdir = ${PWD}
checklib_LTLIBRARIES = libbaz.la libgncmodbaz.la

libbaz_la_SOURCES = baz.c

noinst_HEADERS = \
  baz.h \
  baz-gwrap.h

libgncmodbaz_la_SOURCES = baz-gwrap.c gnc-mod-baz.c

libgncmodbaz_la_LIBADD = ../mod-foo/libfoo.la libbaz.la

EXTRA_DIST = \
  baz-gwrap.scm \
  gnucash/baz.scm

baz-gwrap.c baz-gwrap.h: baz-gwrap.scm ${top_builddir}/config.status
	FLAVOR=gnome $(GUILE) -c \
	  "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	  (primitive-load \"${srcdir}/baz-gwrap.scm\") \
	  (gw:generate-wrapset \"baz-gwrap\")"

BUILT_SOURCES = baz-gwrap.c baz-gwrap.h

DISTCLEANFILES = baz-gwrap.html

# Keep automake from generating install targets for test libs
install-checklibLTLIBRARIES:
uninstall-checklibLTLIBRARIES:
