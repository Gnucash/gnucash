
srcdir   = .
CC       = gcc
INCLPATH = -I/usr/include \
           -I/usr/X11R6/include/. \
           -I./../include \
           -I./../lib/ComboBox-1.33 \
           -I./../lib/XmHTML-1.1.0/src \
           -I./../lib/Xbae-4.6.2-linas 

# libhtmlw eliminated due to license restrictions 
# and general brokenness
#           -I./../lib/libhtmlw 

CFLAGS   = -g -DCELL_WIDGETS=1
LFLAGS   = -O2
LIBS     = -lXpm -lXm -lXmu -lXt -lXext  -lSM -lICE -lX11  -lpng -ljpeg -lz -lm
LIBPATH  = -L/lib -L/usr/lib -L/usr/X11R6/lib/.
TARGET   = xacc
STATIC   = xacc-static

# LIBHTMLW = ../lib/libhtmlw/libhtmlw.a
LIBXMHTML= ../lib/XmHTML-1.1.0/src/libXmHTML.a
LIBXBAE  = ../lib/Xbae-4.6.2-linas/libXbae.a
LIBCOMBO = ../lib/ComboBox-1.33/libComboBox.a
######################################################################
SRCS = cell.c price.c single.c
OBJS = ${SRCS:.c=.o} 
######################################################################

default: $(TARGET)

$(TARGET): $(OBJS)
	@echo "++++++"
	$(CC) $(LFLAGS) $(OBJS) $(LIBPATH) $(LIBS) -o $@

static: $(STATIC)

$(STATIC): $(OBJS)
	@echo "++++++"
	$(CC) $(LFLAGS) $(OBJS) $(LIBPATH) $(LIBS) -o $@ -static

.c.o:
	@echo "+++"
	$(CC) -c $(CFLAGS) $(INCLPATH) $<

depend:
	makedepend -- $(INCLPATH) $(DEFN) -- $(SRCS)

clean:
	rm -f *.o *~ *.bak

distclean: clean
	rm -f $(TARGET) Makefile Makefile.bak config.h

# DO NOT DELETE THIS LINE -- make depend depends on it.
