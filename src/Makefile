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
           -I./engine \
           -I./register \
           -I/usr/local/include \
           -I/usr/X11R6/include/. \
           -I./../include \


CFLAGS   = -g -Wall -ansi -pedantic -Wwrite-strings -Wid-clash-31 -Wpointer-arith -Wcast-qual -Wcast-align -Wtraditional -Wstrict-prototypes -Wmissing-prototypes -Wnested-externs -Wshadow -Woverloaded-virtual -Winline -felide-constructors -fnonnull-objects 
LFLAGS   = -g -Wall
LIBS     = -lXpm -lXm -lXmu -lXt -lXext  -lSM -lICE -lX11  -lpng -ljpeg -lz -lm

######################################################################
SRCS = Ledger.c
OBJS = ${SRCS:.c=.o} 
######################################################################

default: $(OBJS)

motif: default
	@cd engine;            $(MAKE) default
	@cd register;          $(MAKE) motif
	@cd motif;             $(MAKE) motif

gnome: default
	@cd engine;            $(MAKE) default
	@cd register;          $(MAKE) gnome
	@cd gtk;               $(MAKE) gnome

.c.o:
	@echo "+++"
	$(CC) -c $(CFLAGS) $(INCLPATH) $<

depend:
	makedepend -- $(INCLPATH) $(DEFN) -- $(SRCS)
	@cd engine;            $(MAKE) depend
	@cd register;          $(MAKE) depend
	@cd motif;             $(MAKE) depend

clean:
	rm -f *.o *~ *.bak
	@cd engine;            $(MAKE) clean
	@cd gtk;               $(MAKE) clean
	@cd motif;             $(MAKE) clean
	@cd register;          $(MAKE) clean


distclean: clean
	rm -f $(TARGET) $(STATIC) Makefile Makefile.bak config.h
	@cd engine;            $(MAKE) distclean
	@cd gtk;               $(MAKE) distclean
	@cd motif;             $(MAKE) distclean
	@cd register;          $(MAKE) distclean

# DO NOT DELETE THIS LINE -- make depend depends on it.
