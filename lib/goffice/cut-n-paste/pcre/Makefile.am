INCLUDES = \
	$(GNUMERIC_CFLAGS) -I$(top_srcdir) \
	 -DSUPPORT_UTF8 \
	 -DNEWLINE=10 \
	 -DPOSIX_MALLOC_THRESHOLD=100 \
	 -DLINK_SIZE=2 \
	 -DMATCH_LIMIT=10000000

AM_CFLAGS = $(GNOME_CFLAGS)

noinst_LTLIBRARIES = libpcre.la

libpcre_la_SOURCES = maketables.c get.c study.c pcre.c pcreposix.c

noinst_HEADERS = internal.h pcreposix.h pcre.h
