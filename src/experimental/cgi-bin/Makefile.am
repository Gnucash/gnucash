# cgi-bin Makefile.am file.


bin_PROGRAMS = hello hello2

CFLAGS = @CFLAGS@ ${GLIB_CFLAGS} 

LDADD = 			\
  ../../engine/libgncengine.la 	\
  -lxml -lglib

INCLUDES =

hello_SOURCES = \
  hello.c

hello2_SOURCES = \
  hello2.c

noinst_HEADERS = 


EXTRA_DIST = \
  .cvsignore \
  README 

CFLAGS = @CFLAGS@ 

#  glib puts its header files in a bizarre place ...
INCLUDES = 			\
  -I/usr/lib/glib/include 	\
  -I../../engine 
