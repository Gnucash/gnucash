CFLAGS=@CFLAGS@ ${GLIB_CFLAGS} -I../../

checklibdir=${PWD}
checklib_LTLIBRARIES=libfuturemodsys.la libincompatdep.la libagedver.la

libfuturemodsys_la_SOURCES=futuremodsys.c
libfuturemodsys_la_LDFLAGS=-module

libincompatdep_la_SOURCES=incompatdep.c
libincompatdep_la_LDFLAGS=-module

libagedver_la_SOURCES=agedver.c
libagedver_la_LDFLAGS=-module

CLEANFILES += *.so* *.a
