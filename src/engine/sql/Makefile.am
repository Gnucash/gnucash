
# Build the postgres backend as its own loadable shared object.

lib_LTLIBRARIES = libgnc_postgres.la
libgnc_postgres_la_LDFLAGS = -version-info 6:2:6


libgnc_postgres_la_SOURCES = \
  PostgresBackend.c \
  builder.c \
  checkpoint.c \
  gncquery.c \
  kvp-sql.c

noinst_HEADERS = \
  PostgresBackend.h \
  builder.h \
  gncquery.h \
  kvp-sql.h \
  putil.h

EXTRA_DIST = \
  README \
  base-objects.m4 \
  check-objects.m4 \
  design.txt \
  kvp-objects.m4 \
  table.m4 \
  table-create.sql \
  table-drop.sql \
  demo.c 

INCLUDES = -I.. -I/usr/lib/glib/include

LDFLAGS= -lpq

CFLAGS = @CFLAGS@ ${GLIB_CFLAGS}

LDADD = ${GLIB_LIBS}

# Some of the required C files are built with the m4 pre-processor
# As a result, we need to manually specify dependencies, clean targets.

PostgresBackend.o: base-autogen.c table-create.c table-drop.c
PostgresBackend.lo: base-autogen.c table-create.c table-drop.c

checkpoint.o: check-autogen.c
checkpoint.lo: check-autogen.c

kvp-sql.o: kvp-autogen.c
kvp-sql.lo: kvp-autogen.c

base-autogen.c:	table.m4 base-objects.m4
	m4 base-objects.m4 > base-autogen.c

check-autogen.c: table.m4 check-objects.m4
	m4 check-objects.m4 > check-autogen.c

kvp-autogen.c:	table.m4 kvp-objects.m4
	m4 kvp-objects.m4 > kvp-autogen.c

table-drop.c:	table-drop.sql
	echo \" > table-drop.c
	cat table-drop.sql >> table-drop.c
	echo \" >> table-drop.c

table-create.c:	table-create.sql
	echo \" > table-create.c
	cat table-create.sql >> table-create.c
	echo \" >> table-create.c


clean:
	-rm -f base-autogen.c check-autogen.c kvp-autogen.c 
	-rm -f table-drop.c table-create.c

