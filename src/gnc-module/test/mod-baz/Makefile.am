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
libbaz_la_LIBADD = ../mod-foo/libfoo.la

noinst_HEADERS = \
  baz.h

libgncmodbaz_la_SOURCES = gnc-mod-baz.c
nodist_libgncmodbaz_la_SOURCES = baz-gwrap.c

libgncmodbaz_la_LIBADD = \
  libbaz.la \
  ../../libgncmodule.la \
  $(G_WRAP_LINK_ARGS) \
  $(GLIB_LIBS)

EXTRA_DIST = \
  baz-gwrap.scm \
  gnucash/baz.scm

baz-gwrap.c baz-gwrap.h: baz-gwrap.scm ${top_builddir}/config.status
	FLAVOR=gnome $(GUILE) -c \
	  "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	  (primitive-load \"${srcdir}/baz-gwrap.scm\") \
	  (gw:generate-wrapset \"baz-gwrap\")"

BUILT_SOURCES = baz-gwrap.c baz-gwrap.h
CLEANFILES = $(BUILT_SOURCES) baz-gwrap.html

# Keep automake from generating install targets for test libs
install-checklibLTLIBRARIES:
uninstall-checklibLTLIBRARIES:
