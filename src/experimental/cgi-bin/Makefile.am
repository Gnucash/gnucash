# cgi-bin Makefile.am file.


bin_PROGRAMS = hello hello2 fastcgi-hello gnc-server

CFLAGS = @CFLAGS@ ${GLIB_CFLAGS} 

LDADD = 			\
  ../../engine/libgncengine.la 	\
  -lxml -lghttp -lglib -lfcgi

INCLUDES =

hello_SOURCES = \
  hello.c

hello2_SOURCES = \
  hello2.c

fastcgi_hello_SOURCES = \
  fastcgi-hello.c

gnc_server_SOURCES = \
  gnc-server.c

noinst_HEADERS = 


EXTRA_DIST = \
  .cvsignore \
  README 

CFLAGS = @CFLAGS@ 

#  glib puts its header files in a bizarre place ...
INCLUDES = 			\
  -I/usr/lib/glib/include 	\
  -I../../engine 
