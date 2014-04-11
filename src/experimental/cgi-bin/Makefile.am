# cgi-bin Makefile.am file.


bin_PROGRAMS = hello hello2 fastcgi-hello hello3 gnc-server

AM_CFLAGS = \
	-I../../engine -I${srcdir}/../../engine \
	-I../../backend/xml -I${srcdir}/../../backend/xml \
	${GLIB_CFLAGS}

LDADD = 			\
  ../../engine/libgncengine.la 	\
  -lxml -lghttp -lglib -lfcgi -ldl

hello_SOURCES = \
  hello.c

hello2_SOURCES = \
  hello2.c

hello3_SOURCES = \
  hello3.c

fastcgi_hello_SOURCES = \
  fastcgi-hello.c

gnc_server_SOURCES = \
  gnc-server.c

noinst_HEADERS = 


EXTRA_DIST = \
  README \
  login.html
