# cgi-bin Makefile.am file.


bin_PROGRAMS = hello hello2 fastcgi-hello hello3 gnc-server

EXTRA_DIST = .cvsignore

CFLAGS = ${GLIB_CFLAGS} 

LDADD = 			\
  ../../engine/libgncengine.la 	\
  -lxml -lghttp -lglib -lfcgi -ldl

#INCLUDES =

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


EXTRA_DIST += \
  .cvsignore \
  README 

#CFLAGS = @CFLAGS@ 

#  glib puts its header files in a bizarre place ...
INCLUDES = 			\
  -I/usr/lib/glib/include 	\
  -I../../engine 
