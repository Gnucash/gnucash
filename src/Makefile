######################################################################
#********************************************************************
#* Makefile -- makefile for xacc (X-Accountant)                     *
#* Copyright (C) 1997 Robin D. Clark                                *
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
#*   Author: Rob Clark                                              *
#* Internet: rclark@cs.hmc.edu                                      *
#*  Address: 609 8th Street                                         *
#*           Huntington Beach, CA 92648-4632                        *
#********************************************************************

######################################################################
# DO NOT EDIT THE STUFF BELOW THIS LINE!                             #

OBJS = main.o util.o date.o MainWindow.o RegWindow.o BuildMenu.o \
       AccWindow.o FileIO.o Account.o Data.o XferWindow.o FileBox.o \
       QuickFill.o Reports.o RecnWindow.o HelpWindow.o AdjBWindow.o

SRCS = ${OBJS:.o=.c}

TARGET = ../xacc

default: $(TARGET)

$(TARGET): $(OBJS) ../libhtmlw/libhtmlw.a
	@echo "++++++"
	$(CC) $(OBJS) $(LFLAGS) $(LIBS) ../libhtmlw/libhtmlw.a -o $@

.c.o: Makefile
	@echo "+++"
	$(CC) $(CFLAGS) -c $<

depend:
	makedepend -- $(CFLAGS) -- $(SRCS)

clean:
	rm -f *.o *~ *.bak

really_clean:
	makedepend -- --

# DO NOT DELETE THIS LINE -- make  depend  depends  on it.
