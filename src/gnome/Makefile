# Generated automatically from Makefile.in by configure.
#
######################################################################
#********************************************************************
#* Makefile -- makefile for xacc/src/gtk                            *
#* Copyright (C) 1997 Robin Clark                                   *
#*                                                                  *
#* This program is free software; you can redistribute it and/or    *
#* modify it under the terms of the GNU General Public License as   *
#* published by the Free Software Foundation; either version 2 of   *
#* the License, or (at your option) any later version.              *
#*                                                                  *
#* This program is distributed in the hope that it will be useful,  *
#* but WITHOUT ANY WARRANTY; without even the implied warranty of   *
#* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
#* GNU General Public License for more details.                     *
#*                                                                  *
#* You should have received a copy of the GNU General Public License*
#* along with this program; if not, write to the Free Software      *
#* Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
#*                                                                  *
#*   Author: Robin Clark                                            *
#* Internet: rclark@rush.aero.org                                   *
#*  Address: 609 8th Street                                         *
#*           Huntington Beach, CA 92648-4632                        *
#********************************************************************

srcdir   = .
CC       = gcc
INCLPATH = -I/usr/include \
           -I.. \
           -I../.. \
           -I../engine \
           -I../register \
           -I/usr/local/include \
           -I/usr/X11R6/include/. \
           -I./../../include 

# libhtmlw eliminated due to license restrictions 
# and general brokenness
#           -I./../../lib/libhtmlw 

CFLAGS   = -g -Wall
LFLAGS   = -g -Wall
# LIBS     = -lXpm -lXm -lXmu -lXt -lXext  -lSM -lICE -lX11  -lpng -ljpeg -lz -lm
LIBS     = -lgtk -lgdk -lglib -lXpm -lXext -lXmu -lXt -lXext  -lSM -lICE -lX11  -lpng -ljpeg -lz -lm
LIBPATH  = -L/lib -L/usr/lib -L/usr/local/lib -L/usr/X11R6/lib/.
TARGET   = ../../xacc.gtk.bin

LIBREG   = ../libregister.a
LIBENG   = ../libengine.a
######################################################################
SRCS = main.c MainWindow.c MenuBar.c 

# SRCS = AccWindow.c AccountMenu.c AdjBWindow.c    \
#        BuildMenu.c Destroy.c FileBox.c HelpWindow.c  \
#        RecnWindow.c RegWindow.c Reports.c TextBox.c  \
#        XferWindow.c date.c xtutil.c
# OBJS = ${SRCS:.c=.o} $(LIBENG) $(LIBREG) ../Ledger.o 
OBJS = ${SRCS:.c=.o} $(LIBENG)  
######################################################################

default: $(TARGET)

gnome: default

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
	rm -f $(TARGET) $(STATIC) Makefile Makefile.bak config.h

# DO NOT DELETE THIS LINE -- make depend depends on it.
