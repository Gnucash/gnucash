# Generated automatically from Makefile.in by configure.
#
######################################################################
#********************************************************************
#* Makefile -- makefile for xacc/src/engine                         *
#* Copyright (C) 1997 Robin Clark                                   *
#* Copyright (C) 1998 Linas Vepstas                                 *
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
RANLIB   = ranlib
INCLPATH = -I/usr/include \
           -I/usr/local/include \
           -I./../../include \
           -I./../..

CFLAGS   = -g -Wall 
LFLAGS   = -g -Wall
LIBS     = -lXpm -lXm -lXmu -lXt -lXext  -lSM -lICE -lX11  -lpng -ljpeg -lz -lm
LIBPATH  = -L/lib -L/usr/lib -L/usr/local/lib 
TARGET   = ../libengine.a

######################################################################
SRCS = Account.c FileIO.c Group.c LedgerUtils.c QIFIO.c \
       Transaction.c date.c util.c
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

swig:
	swig -I. --guile swig.i


depend:
	makedepend -- $(INCLPATH) $(DEFN) -- $(SRCS)

clean:
	rm -f *.o *~ *.bak

distclean: clean
	rm -f $(TARGET) Makefile Makefile.bak config.h

# DO NOT DELETE THIS LINE -- make depend depends on it.
