SUBDIRS = . 

pkglib_LTLIBRARIES = libgncmod-backend-net.la

AM_CFLAGS = \
  -I.. -I../.. \
  -I../../engine \
  -I../../gnc-module \
  ${GLIB_CFLAGS}

libgncmod_backend_net_la_SOURCES = \
  NetIO.c 

noinst_HEADERS = \
  NetIO.h

libgncmod_backend_net_la_LDFLAGS = -module
libgncmod_backend_net_la_LIBADD = \
   ${GLIB_LIBS}  \
   -L../../engine -L../../engine/.libs -lgncmod-engine
