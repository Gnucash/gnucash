CFLAGS = @CFLAGS@ ${GLIB_CFLAGS} ${G_WRAP_COMPILE_ARGS} -I../mod-foo -I../..

checklibdir = ${PWD}
checklib_LTLIBRARIES = libbaz.la libgncmodbaz.la

libbaz_la_LDFLAGS = -module
libbaz_la_SOURCES = baz.c

noinst_HEADERS = baz.h

libgncmodbaz_la_SOURCES = baz-gwrap.c gnc-mod-baz.c
libgncmodbaz_la_LDFLAGS = -module -L../mod-foo

libgncmodbaz_la_LIBADD = \
  -L. -L./.libs \
  -L../mod-foo -L../mod-foo/.libs \
  -lbaz -lfoo

EXTRA_DIST = \
  baz-gwrap.scm \
  gnucash/baz.scm

baz-gwrap.c baz-gwrap.h: baz-gwrap.scm
	FLAVOR=gnome guile -c \
	  "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	  (primitive-load \"./baz-gwrap.scm\") \
	  (gw:generate-module \"baz-gwrap\")"

BUILT_SOURCES = baz-gwrap.c baz-gwrap.h
CLEANFILES = *.so* *.a baz-gwrap.c baz-gwrap.h baz-gwrap.html
