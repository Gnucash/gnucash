# Generated automatically from Makefile.in by configure.
# Makefile -- makefile for xacc/src/engine                         
# Copyright (C) 1997 Robin Clark                                   
# Copyright (C) 1998 Linas Vepstas                                 
# Copyright (C) 1998 Rob Browning <rlb@cs.utexas.edu>
#                                                                  
# This program is free software; you can redistribute it and/or    
# modify it under the terms of the GNU General Public License as   
# published by the Free Software Foundation; either version 2 of   
# the License, or (at your option) any later version.              
#                                                                  
# This program is distributed in the hope that it will be useful,  
# but WITHOUT ANY WARRANTY; without even the implied warranty of   
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    
# GNU General Public License for more details.                     
#                                                                  
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software      
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        
#                                                                  
#   Author: Robin Clark                                            
# Internet: rclark@rush.aero.org                                   
#  Address: 609 8th Street                                         
#           Huntington Beach, CA 92648-4632                        

# These three lines are suggested defs for autoconf (see the info pages).

prefix   = /usr/local
srcdir   = .
includedir=${prefix}/include


CC       = gcc
INCLPATH = -I.. -I../.. -I../engine -I../register -I./../../include \
           -I$(includedir) -I$(prefix)/lib/gnome-libs/include
 

CFLAGS   =  -O2 -Wall -g -Wall  -I/usr/X11R6/include ${INCLPATH}
LDFLAGS  =  -g -Wall
LIBS     = -L$(prefix)/lib -lpng -ljpeg -lz -lm  -lSM -lICE -lXpm  -L/usr/X11R6/lib \
           $(shell gtk-config --libs)  -lgnomeui -lgnome \
           -lgnomesupport -lintl -lgdk_imlib -ltiff


TARGET   = ../../xacc.gtk.bin
STATIC   = ../../xacc-static.gtk.bin

OTHER_OBJS := ../obj/gnome/*.o ../engine/obj/*.o ../register/obj/gnome/*.o

######################################################################
# See Makefile.common for information about these variables.
GNOME_SRCS := main.c MainWindow.c MenuBar.c RegWindow.c Add_Dialog.c xtutil.c \
              RecnWindow.c
#        AccWindow.c AccountMenu.c AdjBWindow.c    \
#        BuildMenu.c Destroy.c FileBox.c HelpWindow.c  \
#        RecnWindow.c RegWindow.c Reports.c TextBox.c  \
#        XferWindow.c date.c xtutil.c
######################################################################

default: $(TARGET)

# This inclusion must come after the first target, and after the
# definitions of *_SRCS, etc., but before the usage of *_OBJS.
include ../../Makefile.common

gnome: default

static: $(STATIC)

$(TARGET): ${GNOME_OBJS} ${OTHER_OBJS}
	$(CC) $(LDFLAGS) $(LIBS) -o $@ $^

$(STATIC): ${GNOME_OBJS} ${OTHER_OBJS}
	$(CC) -static $(LDFLAGS) $(LIBS) -o $@ $^
