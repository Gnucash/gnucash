
srcdir   = .
CC       = gcc
RANLIB   = ranlib
INCLPATH = -I/usr/include \
           -I/usr/X11R6/include/. \
           -I./../.. \
           -I./../../include \
           -I./../engine \
           -I./../../lib/ComboBox-1.33 \
           -I./../../lib/Xbae-4.6.2-linas 

CFLAGS   = -g -DCELL_WIDGETS=1
LFLAGS   = -O2
TARGET   = ../libregister.a

######################################################################
SRCS = 	basiccell.c cellblock.c combocell.c \
	datecell.c pricecell.c QuickFill.c quickfillcell.c \
	recncell.c register.c table.c textcell.c
OBJS = ${SRCS:.c=.o} 
######################################################################

default: $(TARGET)

$(TARGET): $(OBJS)
	@echo "++++++"
	$(AR) rv $(TARGET) $(OBJS)
	$(RANLIB) $(TARGET)

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
