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

OBJS = Account.o AccWindow.o Action.o AdjBWindow.o BuildMenu.o Data.o date.o \
       FileBox.o FileIO.o HelpWindow.o main.o MainWindow.o \
       QIFIO.o QuickFill.o RecnWindow.o RegWindow.o Reports.o \
       Transaction.o util.o XferWindow.o

SRCS = ${OBJS:.o=.c}

TARGET = ../xacc

default: $(TARGET)

$(TARGET): $(OBJS) ../lib/libhtmlw.a ../lib/libComboBox.a ../Xbae-4.6.2-linas/src/libXbae.a
	@echo "++++++"
	$(CC) $(OBJS) $(LFLAGS) -lhtmlw -lComboBox \
        ../Xbae-4.6.2-linas/src/libXbae.a $(LIBS) -o $@

.c.o: Makefile
	@echo "+++"
	$(CC) $(CFLAGS) -c $<

depend:
	makedepend -- $(CFLAGS) -- $(SRCS)

clean:
	rm -f tmp junk glop core *.o *~ *.bak

really_clean:
	makedepend -- --

# DO NOT DELETE THIS LINE -- make  depend  depends  on it.
