CFLAGS=@CFLAGS@ ${GLIB_CFLAGS} ${G_WRAP_COMPILE_ARGS}

INCLUDES=-I${top_srcdir}/src/gnc-module

checklibdir=${PWD}
checklib_LTLIBRARIES = libbar.la libgncmodbar.la

libbar_la_LDFLAGS = -module
libbar_la_SOURCES = bar.c

libgncmodbar_la_SOURCES=bar-gwrap.c gnc-mod-bar.c
libgncmodbar_la_LDFLAGS=-module
libgncmodbar_la_LIBADD=-L. -L./.libs -lbar

bar-gwrap.c bar-gwrap.h: bar-gwrap.scm
	FLAVOR=gnome guile -c \
	  "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	  (primitive-load \"./bar-gwrap.scm\") \
	  (gw:generate-module \"bar-gwrap\")"

BUILT_SOURCES += bar-gwrap.c bar-gwrap.h
CLEANFILES += *.so* *.a bar-gwrap.c bar-gwrap.h bar-gwrap.html
