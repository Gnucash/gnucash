CFLAGS=@CFLAGS@ ${GLIB_CFLAGS} ${G_WRAP_COMPILE_ARGS}

INCLUDES=-I${top_srcdir}/src/gnc-module

checklibdir=${PWD}
checklib_LTLIBRARIES=libfoo.la libgncmodfoo.la

libfoo_la_LDFLAGS = -module
libfoo_la_SOURCES = foo.c

libgncmodfoo_la_SOURCES = foo-gwrap.c gnc-mod-foo.c
libgncmodfoo_la_LDFLAGS = -module -L.libs
libgncmodfoo_la_LIBADD = -lfoo

foo-gwrap.c foo-gwrap.h: foo-gwrap.scm
	FLAVOR=gnome guile -c \
	  "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	  (primitive-load \"./foo-gwrap.scm\") \
	  (gw:generate-module \"foo-gwrap\")"

BUILT_SOURCES += foo-gwrap.c foo-gwrap.h
CLEANFILES += *.so* *.a foo-gwrap.c foo-gwrap.h foo-gwrap.html
