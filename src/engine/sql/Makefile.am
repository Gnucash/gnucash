
# Build the postgres backend as its own loadable shared object.

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

LDFLAGS= -lpq

# Some of the required C files are built with the m4 pre-processor

PostgresBackend.o: autogen.c
PostgresBackend.lo: autogen.c

autogen.c:	table.m4
	m4 table.m4 > autogen.c
