# Generated automatically from Makefile.in by configure.
#
######################################################################
#********************************************************************
#* Makefile -- makefile for xacc/src                                *
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
           -I/usr/X11R6/include/. \
           -I./../include \
           -I./../lib/ComboBox-1.33 \
           -I./../lib/XmHTML-1.1.0/src \
           -I./../lib/Xbae-4.6.2-linas 

# libhtmlw eliminated due to license restrictions 
# and general brokenness
#           -I./../lib/libhtmlw 

CFLAGS   = -O2 -DCELL_WIDGETS=1
LFLAGS   = -O2
LIBS     = -lXpm -lXm -lXmu -lXt -lXext  -lSM -lICE -lX11  -lpng -ljpeg -lz -lm
LIBPATH  = -L/lib -L/usr/lib -L/usr/X11R6/lib/.
TARGET   = ../xacc

# LIBHTMLW = ../lib/libhtmlw/libhtmlw.a
LIBXMHTML= ../lib/XmHTML-1.1.0/src/libXmHTML.a
LIBXBAE  = ../lib/Xbae-4.6.2-linas/libXbae.a
LIBCOMBO = ../lib/ComboBox-1.33/libComboBox.a
######################################################################
SRCS = AccWindow.c Account.c AccountMenu.c Action.c AdjBWindow.c    \
       BuildMenu.c Data.c Destroy.c FileBox.c FileIO.c HelpWindow.c \
       LedgerUtils.c MainWindow.c PopBox.c QIFIO.c QuickFill.c      \
       RecnWindow.c RegWindow.c Reports.c TextBox.c Transaction.c   \
       XferBox.c XferWindow.c date.c main.c util.c
# OBJS = ${SRCS:.c=.o} $(LIBHTMLW) $(LIBXBAE) $(LIBCOMBO)
OBJS = ${SRCS:.c=.o} $(LIBXMHTML) $(LIBXBAE) $(LIBCOMBO)
######################################################################

default: $(TARGET)

$(TARGET): $(OBJS)
	@echo "++++++"
	$(CC) $(LFLAGS) $(OBJS) $(LIBPATH) $(LIBS) -o $@

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
