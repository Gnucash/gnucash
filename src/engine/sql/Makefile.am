
# Build the postgres backend as its own loadable shared object.

lib_LTLIBRARIES = libgnc_postgres.la
libgnc_postgres_la_LDFLAGS = -version-info 5:2:5


libgnc_postgres_la_SOURCES = \
  builder.c \
  checkpoint.c \
  gncquery.c \
  PostgresBackend.c 


EXTRA_DIST = \
  builder.h \
  design.txt \
  PostgresBackend.h \
  README \
  table.m4 \
  table-create.sql \
  table-drop.sql \
  demo.c 

INCLUDES = -I.. -I/usr/lib/glib/include

LDFLAGS= -lpq

# Some of the required C files are built with the m4 pre-processor

PostgresBackend.o: autogen.c table-create.c table-drop.c
PostgresBackend.lo: autogen.c table-create.c table-drop.c

autogen.c:	table.m4
	m4 table.m4 > autogen.c

table-drop.c:	table-drop.sql
	echo \" > table-drop.c
	cat table-drop.sql >> table-drop.c
	echo \" >> table-drop.c

table-create.c:	table-create.sql
	echo \" > table-create.c
	cat table-create.sql >> table-create.c
	echo \" >> table-create.c
