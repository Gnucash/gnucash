
# This isn't right right now.  This needs to be updated to be a real
# Makefile.am...

lib_LTLIBRARIES = libgnc_postgres.la

libgnc_postgres_la_SOURCES = \
  builder.c \
  gncquery.c \
  PostgresBackend.c 


EXTRA_DIST = \
  builder.h \
  PostgresBackend.h \
  README \
  demo.c \
  gnc-init.sh \
  gnc-init.sql 

INCLUDES = -I.. -I/usr/lib/glib/include
